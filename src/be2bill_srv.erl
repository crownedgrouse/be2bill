%%%-----------------------------------------------------------------------------
%%% File:      be2bill_srv.erl
%%% @author    Eric Pailleau <be2bill@crownedgrouse.com>
%%% @copyright 2016 crownedgrouse.com
%%% @doc
%%% Gen_server for Be2bill API
%%% @end
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2016-10-16
%%%-----------------------------------------------------------------------------
-module(be2bill_srv).
-author("Eric Pailleau <be2bill@crownedgrouse.com>").
-behaviour(gen_server).

% Do not warn deprecated gen_fsm, as gen_statem used if possible
-compile([{nowarn_deprecated_function, [{gen_fsm, sync_send_event, 2}]}]).

-include("be2bill_defs.hrl").

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



%% Store some stats for live metrics
-record(state,
{'buildPaymentFormButton'         = 0 :: non_neg_integer()
,'buildAuthorizationFormButton'   = 0 :: non_neg_integer()
,'capture'                        = 0 :: non_neg_integer()
,'refund'                         = 0 :: non_neg_integer()
,'oneClickPayment'                = 0 :: non_neg_integer()
,'oneClickAuthorization'          = 0 :: non_neg_integer()
,'subscriptionPayment'            = 0 :: non_neg_integer()
,'subscriptionAuthorization'      = 0 :: non_neg_integer()
,'redirectForPayment'             = 0 :: non_neg_integer()
,'stopNTimes'                     = 0 :: non_neg_integer()
,'payment'                        = 0 :: non_neg_integer()
,'authorization'                  = 0 :: non_neg_integer()
,'getTransactionsByOrderId'       = 0 :: non_neg_integer()
,'getTransactionsByTransactionId' = 0 :: non_neg_integer()
,'exportTransactions'             = 0 :: non_neg_integer()
,'exportChargebacks'              = 0 :: non_neg_integer()
,'exportReconciliation'           = 0 :: non_neg_integer()
,'exportReconciledTransactions'   = 0 :: non_neg_integer()
}).

%% Macros
% Increment method call counter in state
-define(INC_STATE(S, R), S{R = ( S.R + 1)}).

% Common treatment to all methods
-define(COMMON(R, Store),
   case be2bill_lib:check_record(R) of
      {_, {error, Err}} -> {error, Err} ;
      {N, {ok, Ok}}     ->
                           % Add dynamic http fsm to handle the request
                           {ok, Child} = supervisor:start_child(get(httpfsm), []),
                           Password = get('password'),
                           {ok, Post} = be2bill_lib:compute_post({N, Ok}, Password),
                           case Store of
                                 true -> true = dets:insert_new(get(store), { erlang:phash2(Post), Post}) ;
                                 _    -> ok
                           end,
                           finite_state(Child, Post)
   end
).

%% API.
-spec start_link(list()) -> {ok, pid()}.

start_link(ConfigName) ->
    Envs = application:get_env(be2bill, ConfigName, []),
    Name = proplists:get_value('name', Envs, {local, ConfigName}),
    gen_server:start_link(Name, ?MODULE, [ConfigName], []).

%% gen_server.

%%******************************************************************************
%% Supervisor will start both production and sandbox env.
%% Each server instance will decide to stop or start depending its own env config.
init([ConfigName]) ->
	 Env  = case lists:member(ConfigName, application:get_env(be2bill, prod_env, [])) of
					 true  -> 'prod_env' ;
					 false ->  case lists:member(ConfigName, application:get_env(be2bill, dev_env, [])) of
										true -> 'dev_env' ;
										false -> 'WTF'
								  end
			  end,
   % Disable any tracing/debugging of this process when production mode
   % This way sensitive data cannot be stolen at runtime.
   Sec = case Env of
            'prod_env' -> true ;
            'dev_env'  -> false ;
            _          -> exit("Something weird. Internal environments can only be 'prod_env' or 'dev_env'.")
         end,
   erlang:process_flag(sensitive, Sec),
   % Check in application config if current env type can be used,
   % otherwise stop cleanly
   Start = lists:member(Env, application:get_env(be2bill, env, [])),
   case Start of
      true ->  % Start the http supervisor under this gen_server,
               % where http fsm will be dynamically added by this gen_server
               Name = {local, list_to_atom(atom_to_list(ConfigName) ++ "_fsm_sup") },
               update_config(ConfigName),
               {ok, HttpFsm} = supervisor:start_link(Name, be2bill_simplesup, [ConfigName]),
               put(httpfsm, HttpFsm), %  Store PID
               % Open dets disc store (in priv/ directory if not set)
               DetsDir = application:get_env(be2bill, vault, code:priv_dir('be2bill')),
               case dets:open_file(filename:join(DetsDir, atom_to_list(ConfigName) ++ ".dets"), []) of
                    {ok, Reference}  ->
                         put(store, Reference),
                         % Resume stored requests (unfinished) TODO logging that some request are resumed
                         dets:traverse(Reference,
                                       fun({Key, Post}) ->
                                          {ok, Child} = supervisor:start_child(get(httpfsm), []),
                                          finite_state(Child, Post),
                                          io:format("Resuming uncommited transaction : ~p~n",[Key]),
                                          continue end) ;
                    {error, _Reason} -> % TODO logging
                                        ok
               end,
               {ok, #state{}};
      _    -> ignore
   end.

%%==============================================================================
%% Erlang API method (not be2bill)
%%
%%------------------------------------------------------------------------------
%% 'metrics' Return State and reset a new one
%% It is up to the caller to perform this at his prefered frequency.
%% This give statistics and can also help to raise alarms
%% (for instance abnormal decrease of some methods calls which may signal
%%  an issue, like web site unavailable, DDOS and so on ...)
%%------------------------------------------------------------------------------
handle_call('metrics', _From, State)
   ->
	{reply, State, #state{}};

%%==============================================================================
%% @doc Synchronous request on official be2bill methods
%%      https://developer.be2bill.com/functions/
%% @end
%%------------------------------------------------------------------------------
%%  buildPaymentFormButton
%%      This function is used to display a cart validation button
%%      to call the payment form.
%%      https://developer.be2bill.com/functions/buildPaymentFormButton
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'buildPaymentFormButton')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'buildPaymentFormButton') };
%%------------------------------------------------------------------------------
%%  buildAuthorizationFormButton
%%      This function is used to display a cart validation button
%%      to call the form for an authorization.
%%      This authorization can be captured with the function : capture .
%%      https://developer.be2bill.com/functions/buildAuthorizationFormButton
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'buildAuthorizationFormButton')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'buildAuthorizationFormButton')};
%%------------------------------------------------------------------------------
%%  capture
%%      This authorization can be captured with this function.
%%      The 'capture' takes place after the following function:
%%      'buildAuthorizationFormButton' and allows to collect the carholder's
%%      funds up to 7 days.
%%      https://developer.be2bill.com/functions/capture
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'capture')
	->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'capture')};

%%------------------------------------------------------------------------------
%%  refund
%%      A refund requires the intital transaction ID to be called back.
%%      If a refund is operated during the same day as the initial payment,
%%      it will be a simple payment cancellation and no remote collection
%%      will be made.
%%      https://developer.be2bill.com/functions/refund
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'refund')
	->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'refund')};

%%------------------------------------------------------------------------------
%%  oneClickPayment
%%      You may debit a cardholder without having him to re-enter his sensitive
%%      details (card numbers, CVC, etc.) through the use of an ALIAS.
%%      It allows you to implement one click payments in a secure way
%%      (in accordance with the PCI-DSS security norm).
%%      https://developer.be2bill.com/functions/oneClickPayment
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'oneClickPayment')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'oneClickPayment')};

%%------------------------------------------------------------------------------
%%  oneClickAuthorization
%%      You may do an authization on a holder's card without having him
%%      to re-enter his sensitive details (card numbers, CVC, etc.) through
%%      the use of an ALIAS.
%%      It allows you to implement one click payments in a secure way
%%      (in accordance with the PCI-DSS security norm).
%%      https://developer.be2bill.com/functions/oneClickAuthorization
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'oneClickAuthorization')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'oneClickAuthorization')};

%%------------------------------------------------------------------------------
%%  subscriptionPayment
%%      You may debit a cardholder without having him to re-enter his sensitive
%%      details (card numbers, CVC, etc.) through the use of an ALIAS.
%%      It allows you to implement subscription payments in a secure way
%%      (in accordance with the PCI-DSS security norm).
%%      https://developer.be2bill.com/functions/subscriptionPayment
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'subscriptionPayment')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'subscriptionPayment')};

%%------------------------------------------------------------------------------
%%  subscriptionAuthorization
%%      You may do an authization on a holder's card without having him to
%%      re-enter his sensitive details (card numbers, CVC, etc.) through
%%      the use of an ALIAS.
%%      It allows you to implement subscription payments in a secure way
%%      (in accordance with the PCI-DSS security norm).
%%      https://developer.be2bill.com/functions/subscriptionAuthorization
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'subscriptionAuthorization')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'subscriptionAuthorization')};

%%------------------------------------------------------------------------------
%%  redirectForPayment
%%       This operation allows you to redirect the customer to an alternative
%%       payment method such as PayPal once his cart is validated.
%%      https://developer.be2bill.com/functions/redirectForPayment
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'redirectForPayment')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'redirectForPayment')};

%%------------------------------------------------------------------------------
%%  stopNTimes
%%  The 'stopNTimes' function will allow you to cancel the futures n-Times
%%  schedules. For this, we need to use the initial id.
%%      https://developer.be2bill.com/functions/stopNTimes
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'stopNTimes')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'stopNTimes')};

%%------------------------------------------------------------------------------
%%  payment
%%  This integration method differs from the standard service (form mode) on
%%  several criterions:
%%  The card details (PAN, expiry date, CVC, full name) go through your infrastructure.
%%  The 3D Secure form display must be done by your platform.
%%  Calls to this service use HTTPS requests.
%%  You need to send HTTP requests with the transaction parameters and receive
%%  the result in the request return.
%%      https://developer.be2bill.com/functions/payment
%%------------------------------------------------------------------------------
% DO NO STORE SENSITIVE REQUEST
handle_call(Req, _From, State) when is_record(Req, 'payment'),
                                    ((Req#payment.'CARDPAN' =/= undefined) or
                                     (Req#payment.'CARDCRYPTOGRAM' =/= undefined) or
                                     (Req#payment.'CARDDATE' =/= undefined) or
                                     (Req#payment.'CARDFULLNAME' =/= undefined) )
   ->
   Reply = ?COMMON(Req, false),
	{reply,Reply, ?INC_STATE(State#state, 'payment')};
% USING AN ALIAS CAN BE STORED
handle_call(Req, _From, State) when is_record(Req, 'payment')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'payment')};

%%------------------------------------------------------------------------------
%%  authorization
%%  This integration method differs from the standard service (form mode) on
%%  several criterions:
%%  The card details (PAN, expiry date, CVC, full name) go through your infrastructure.
%%  The 3D Secure form display must be done by your platform.
%%  Calls to this service use HTTPS requests.
%%  You need to send HTTP requests with the transaction parameters and receive
%%  the result in the request return.
%%      https://developer.be2bill.com/functions/authorization
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'authorization')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'authorization')};

%%------------------------------------------------------------------------------
%% getTransactionsByOrderId
%% This function allows you to find one or more transactions with their ORDERID.
%%      https://developer.be2bill.com/functions/getTransactionsByOrderId
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'getTransactionsByOrderId')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'getTransactionsByOrderId')};

%%------------------------------------------------------------------------------
%% getTransactionsByTransactionId
%% This function allows you to find one or more transactions with their TRANSACTIONID.
%%      https://developer.be2bill.com/functions/getTransactionsByTransactionId
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'getTransactionsByTransactionId')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'getTransactionsByTransactionId')};

%%------------------------------------------------------------------------------
%% exportTransactions
%% This function allows you to gather a list of transactions for a given day or month.
%%      https://developer.be2bill.com/functions/exportTransactions
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'exportTransactions')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'exportTransactions')};

%%------------------------------------------------------------------------------
%% exportChargebacks
%% This function allows you to gather a list of chargebacks for a given day or month.
%%      https://developer.be2bill.com/functions/exportChargebacks
%%------------------------------------------------------------------------------
handle_call(Req, _From, State)  when is_record(Req, 'exportChargebacks')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'exportChargebacks')};

%%------------------------------------------------------------------------------
%% exportReconciliation
%% This function allows you to gather the financial reconciliation for a given day or month.
%%      https://developer.be2bill.com/functions/exportReconciliation
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'exportReconciliation')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'exportReconciliation')};

%%------------------------------------------------------------------------------
%% exportReconciledTransactions
%% This function allows you to gather a list of collected transactions for a given day.
%%      https://developer.be2bill.com/functions/exportReconciledTransactions
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'exportReconciledTransactions')
   ->
   Reply = ?COMMON(Req, true),
	{reply,Reply, ?INC_STATE(State#state, 'exportReconciledTransactions')};
%%------------------------------------------------------------------------------
%% Last resort callback : Unknown/invalid calls return error
%%      (black hole)
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	% TODO log errors
	{reply, error, State}.

%%******************************************************************************
handle_cast({commit, ReqRef}, State) ->
   io:format("Commit req : ~p~n", [ReqRef]), % TODO gen_even log
   ok = dets:delete(get(store), ReqRef),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%******************************************************************************
handle_info(_Info, State) ->
	{noreply, State}.

%%******************************************************************************
terminate(_Reason, _State) ->
   dets:sync(get(store)),
   dets:close(get(store)),
	ok.

%%******************************************************************************
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%==============================================================================

update_config(ConfName) when is_atom(ConfName)->
    Envs = application:get_env(be2bill, ConfName, []),
    % Store locally, and hide password
    Password = proplists:get_value('password', Envs, ""),
    put('password', Password),
    New = proplists:delete('password', Envs),
    application:set_env(be2bill, ConfName, New ++ [{'password', "*********"}]),
    Id  = proplists:get_value('identifier', Envs, ""),
    put('identifier', Id),
    % Create main_servers/backup_servers list config
    MainPool       = proplists:get_value('main_pool', Envs, []),
    BackupPool     = proplists:get_value('backup_pool', Envs, []),
    MainServers    = be2bill_netlib:pool2ips(MainPool),
    BackupServers  = be2bill_netlib:pool2ips(BackupPool),
    % Set Ips per environment in config at runtime
    ok= application:set_env(be2bill, list_to_atom(atom_to_list(ConfName) ++ "_net" ) , [{main_servers, MainServers}, {backup_servers, BackupServers}]),

    ok.

%%******************************************************************************
finite_state(Child, Post) ->
   % Use gen_statem if ERLANG > 20.0
   case code:is_loaded(gen_statem) of
      false ->
            gen_fsm:sync_send_event(Child, Post);
      _  ->
            gen_statem:call(Child, Post)
   end.
