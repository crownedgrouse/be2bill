%%%-----------------------------------------------------------------------------
%%% File:      be2bill_fsm.erl
%%% @author    Eric Pailleau <be2bill@crownedgrouse.com>
%%% @copyright 2016 crownedgrouse.com
%%% @doc
%%% Finite state machine for be2bill requests
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
-module(be2bill_fsm).
-behaviour(gen_fsm).

%% API.
-export([start_link/1]).

%% gen_fsm.
-export([init/1]).
-export([prepare/2, main/2, backup/2]).
-export([handle_event/3]).
-export([prepare/3, main/3, backup/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-include("be2bill_statem.hrl").

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(ConfigName) ->
	gen_fsm:start_link(?MODULE, [ConfigName], []).

%% gen_fsm.
%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
init([ConfigName]) ->
	Env  = case lists:member(ConfigName, application:get_env(be2bill, prod_env, [])) of
					 true  -> 'prod_env' ;
					 false ->  case lists:member(ConfigName, application:get_env(be2bill, dev_env, [])) of
										true -> 'dev_env' ;
										false -> 'WTF'
								  end
			  end,
   put(env, Env),
   % Disable any tracing/debugging of this process when production mode
   % This way sensitive data cannot be stolen at runtime.
   Sec = case Env of
            'prod_env' -> true ;
            'dev_env'  -> false ;
            _          -> exit("Something weird. Internal environments can only be 'prod_env' or 'dev_env'.")
         end,
   erlang:process_flag(sensitive, Sec),
   % Compute list of IP available (dynamic as far we can change this at runtime)
   Envs      = application:get_env(be2bill, ConfigName, []),
   NetEnv    = application:get_env(be2bill, list_to_atom(atom_to_list(Env) ++ "_net" ),[]),
   MainIps   = proplists:get_value('main_servers', NetEnv, []),
   BackupIps = proplists:get_value('backup_servers', NetEnv, []),
   Next      = server_pick(MainIps),
   HO        = proplists:get_value('http_options', Envs, []),
   O         = proplists:get_value('req_options', Envs, []),
   GS        = case proplists:get_value('name', Envs, ConfigName) of
                    {local , Name} -> Name ;
                    {global, Name} -> Name ;
                    Name -> Name
               end,
   put(gs, GS), % My associated gen_server
   put(hbw,proplists:get_value('http_basic_wait',   Envs, application:get_env(be2bill, 'http_basic_wait'  , 5))),
   put(hrw,proplists:get_value('http_random_wait',  Envs, application:get_env(be2bill, 'http_random_wait',  2))),
   put(hrs,proplists:get_value('http_random_scale', Envs, application:get_env(be2bill, 'http_random_scale', {1, 5}))),
	{ok, prepare, #state{next=Next, http_options=HO, options=O, main=MainIps, backup=BackupIps}}.

%%------------------------------------------------------------------------------
%% Each http request will be handled by an individual gen_fsm.
%% Each http request will be retried several times on main system,
%% then tried several times on backup system.
%% When a request is OK on a server (after a first error),
%% a message is sent to all other fsm, in order to try this server instead keep
%% going on current invalid one.
%% The goal is to drain the quickest way pending requests, but avoid overload,
%% either locally or at server side, by using a random retry delay.
%%------------------------------------------------------------------------------
% Asynchronous
prepare(Data, StateData) ->
	{next_state, main, StateData#state{post=Data}, 10}.


main(_Event, StateData) ->
   erlang:display("Sorry, please use gen_server:call/2 ."),
   {stop, normal, StateData}.

backup(_Event, StateData) ->
   NextState = todo,
	{next_state, NextState, StateData}.

% Synchonous
handle_event(Event, StateName, StateData) ->
   io:format("~p received : ~p~n",[self(), Event]),
   {next_state, StateName, StateData}.

prepare(Data, From, StateData) ->
   ID = erlang:phash2(Data),
   gen_fsm:reply(From, {ok, ID}),
	{next_state, main, StateData#state{id= ID, post=Data}, sleep_time()}.

main(_Event, From, StateData) -> % Try to post
   io:format("Trying req : ~p on ~p~n",[StateData#state.id,StateData#state.next]),% TODO gen_even log
   % random ok or ko
   case ( rand:uniform() > 0.9 ) of % simulate retries for now TODO
                     true  -> io:format("OK     req : ~p~n",[StateData#state.id]),% TODO gen_even log
                              gen_fsm:reply(From, {ok, StateData#state.id}),
                              {stop, normal, StateData} ;
                     false -> io:format("KO     req : ~p~n",[StateData#state.id]),% TODO gen_even log
                              NewStateData = StateData#state{next=server_pick(StateData#state.main)},
	                           {next_state, main, NewStateData, sleep_time()}
   end.

backup({timeout, _, _}, _From, StateData) ->
	{reply, ignored, backup, StateData};
backup(_Event, _From, StateData) ->
	{reply, ignored, backup, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

%% Receive info from others that a server is ok
handle_info({'alive', Server }, StateName, StateData) ->
	{next_state, StateName, StateData#state{next = Server}}.

terminate(_Reason, _StateName, StateData) ->
   gen_server:cast(get(gs), {commit, erlang:phash2(StateData#state.post)}),
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%===============================================================================

server_pick([]) -> [];
server_pick(L)  -> lists:sublist(L, rand:uniform(length(L)), 1).


%% http_retries      : number of tries on same system (main / backup), then switch.
%% http_basic_wait   : basic wait time
%% http_random_wait  : basic random wait time
%% http_random_scale : random scale tuple  ex : {1,5}
%%
%% Timeout will be computed this way :
%% http_basic_wait + (http_random_wait * rand(http_random_scale))
%% A randomly computed timeout avoid wave effects with huge number of requests
%% at same time.
sleep_time() -> HBW = get(hbw),
                HRW = get(hrw),
                {F, T} = get(hrs),
                HRS = (F + (rand:uniform() * (T - F) )),
                %io:format("~p / ~p / ~p~n",[HBW, HRW, HRS]),
                % Return sleep time in milliseconds
                S = (erlang:round(( HBW + (HRW * HRS))) * 1000) ,
                %io:format("S = ~p~n",[S]),
                S.


