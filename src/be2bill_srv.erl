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
-define(COMMON(R),
    
         case be2bill_lib:check_record(R) of
              {error, Err} -> {error, Err} ;
              {ok, Ok}     -> % Add dynamic http fsm to handle the request
                              {ok, Child} = supervisor:start_child(get(httpfsm), []),
                              gen_fsm:sync_send_event(Child, Ok)
        end
).

%% API.
-spec start_link(list()) -> {ok, pid()}.

start_link(Env) ->
    Envs = application:get_env(be2bill, Env, []),
    Name = proplists:get_value('name', Envs, {local, Env}),
    gen_server:start_link(Name, ?MODULE, Env, []).

%% gen_server.

%%******************************************************************************
%% Supervisor will start both production and sandbox env.
%% Each server instance will decide to stop or start depending its own env config.
init(Env) ->   
   % Disable any tracing/debugging of this process when production mode
   % This way sensitive data cannot be stolen at runtime.
   Sec = case Env of
            'production' -> true ;
            _            -> false 
         end,
   erlang:process_flag(sensitive, Sec),
   % Check in application config if current env can be used,
   % otherwise stop cleanly
   % env = [production, sandbox, both] Default to 'both'.
   Start = case application:get_env(be2bill, env, both) of
               both                 -> true ;
               X when (Env =:= X)   -> true ;
               _                    -> false
        end,
   case Start of
      true ->  % Start the http supervisor under this gen_server, 
               % where http fsm will be dynamically added by this gen_server
               Name = case Env of
                           'production' -> {local, prod_fsm_sup} ;
                           _            -> {local, sand_fsm_sup} 
                      end,
               {ok, HttpFsm} = supervisor:start_link(Name, be2bill_simplesup, [Env]),
               put(httpfsm, HttpFsm), %  Store PID
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
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'subscriptionAuthorization')};

%%------------------------------------------------------------------------------
%%  redirectForPayment
%%       This operation allows you to redirect the customer to an alternative 
%%       payment method such as PayPal once his cart is validated.
%%      https://developer.be2bill.com/functions/redirectForPayment
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'redirectForPayment')
   ->
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'redirectForPayment')};

%%------------------------------------------------------------------------------
%%  stopNTimes
%%  The 'stopNTimes' function will allow you to cancel the futures n-Times
%%  schedules. For this, we need to use the initial id.
%%      https://developer.be2bill.com/functions/stopNTimes
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'stopNTimes') 
   ->
				Reply = ?COMMON(Req), 
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
handle_call(Req, _From, State) when is_record(Req, 'payment') 
   ->
				Reply = ?COMMON(Req), 
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
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'authorization')};

%%------------------------------------------------------------------------------
%% getTransactionsByOrderId
%% This function allows you to find one or more transactions with their ORDERID.
%%      https://developer.be2bill.com/functions/getTransactionsByOrderId
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'getTransactionsByOrderId') 
   ->
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'getTransactionsByOrderId')};

%%------------------------------------------------------------------------------
%% getTransactionsByTransactionId
%% This function allows you to find one or more transactions with their TRANSACTIONID.
%%      https://developer.be2bill.com/functions/getTransactionsByTransactionId
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'getTransactionsByTransactionId') 
   ->
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'getTransactionsByTransactionId')};

%%------------------------------------------------------------------------------
%% exportTransactions
%% This function allows you to gather a list of transactions for a given day or month.
%%      https://developer.be2bill.com/functions/exportTransactions
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'exportTransactions') 
   ->
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'exportTransactions')};

%%------------------------------------------------------------------------------
%% exportChargebacks
%% This function allows you to gather a list of chargebacks for a given day or month.
%%      https://developer.be2bill.com/functions/exportChargebacks
%%------------------------------------------------------------------------------
handle_call(Req, _From, State)  when is_record(Req, 'exportChargebacks') 
   ->
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'exportChargebacks')};

%%------------------------------------------------------------------------------
%% exportReconciliation
%% This function allows you to gather the financial reconciliation for a given day or month.
%%      https://developer.be2bill.com/functions/exportReconciliation
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'exportReconciliation')
   ->
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'exportReconciliation')};

%%------------------------------------------------------------------------------
%% exportReconciledTransactions
%% This function allows you to gather a list of collected transactions for a given day.
%%      https://developer.be2bill.com/functions/exportReconciledTransactions
%%------------------------------------------------------------------------------
handle_call(Req, _From, State) when is_record(Req, 'exportReconciledTransactions')
   ->
				Reply = ?COMMON(Req), 
	{reply,Reply, ?INC_STATE(State#state, 'exportReconciledTransactions')};
%%------------------------------------------------------------------------------
%% Last resort callback : Unknown/invalid calls return error 
%%      (black hole)
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	% TODO log errors
	{reply, error, State}.

%%******************************************************************************
handle_cast(_Msg, State) ->
	{noreply, State}.

%%******************************************************************************
handle_info(_Info, State) ->
	{noreply, State}.

%%******************************************************************************
terminate(_Reason, _State) ->
	ok.

%%******************************************************************************
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
