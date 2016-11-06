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

-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
-type ip6_address() :: {0..65535, 0..65535,
                        0..65535, 0..65535, 
                        0..65535, 0..65535,
                        0..65535, 0..65535}.
-type ip_address() :: ip4_address() | ip6_address().

-record(state, 
{next   = undefined :: undefined | ip_address() % the server to be tried next time
,server = undefined :: undefined | ip_address() % the current server being tried
,tries  = 0   :: non_neg_integer()              % current number of tries
,status = 0   :: non_neg_integer()              % Http code received
,main   = []  :: list()                         % list of main system IPs
,backup = []  :: list()                         % list of backup system IPs
,path         :: string()                       % path part of url
,post         :: string()                       % data to post
}).

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Env) ->
	gen_fsm:start_link(?MODULE, Env, []).

%% gen_fsm.

init(Env) ->
   put(env, Env),
   % Disable any tracing/debugging of this process when production mode
   % This way sensitive data cannot be stolen at runtime.
   Sec = case Env of
            'production' -> true ;
            _            -> false 
         end,
   erlang:process_flag(sensitive, Sec),
	{ok, prepare, #state{}}.

%%------------------------------------------------------------------------------
%% Each http request will be handled by an individual gen_fsm.
%% Each http request will be retried several times on main system,
%% then tried several times on backup system.
%% When a request is OK on a server (after a first error),
%% a message is sent to all other fsm, in order to try this server instead keep 
%% going on current invalid one.
%% The goal is to drain the quickest way pending requests, but avoid overload,
%% either locally or at server side, by using a random retry delay.
%%       Config parameters
%% http_retries      : number of tries on same system (main / backup), then switch.
%% http_basic_wait   : basic wait time
%% http_random_wait  : basic random wait time 
%% http_random_scale : random scale tuple  ex : {1,5} 
%% 
%% Timeout will be computed this way : 
%% http_basic_wait + (http_random_wait * rand(http_random_scale))
%% A randomly computed timeout avoid wave effects with huge number of requests
%% at same time.
%%------------------------------------------------------------------------------
prepare(Data, StateData) ->
   Env = application:get_env(be2bill, get(env), []),
   Password = proplists:get_value('password', Env, ""),
   {ok, Post}     = be2bill_lib:compute_post(Data, Password),
	{next_state, main, StateData#state{post=Post}}.

main(_Event, StateData) ->
   NextState = todo,
	{next_state, NextState, StateData}.

backup(_Event, StateData) ->
   NextState = todo,
	{next_state, NextState, StateData}.

%
handle_event('try', StateName, StateData) ->
   % Perform https request and select next state depending result
	{next_state, StateName, StateData}.

prepare(Data, _From, StateData) ->
   Env = application:get_env(be2bill, get(env), []),
   Password = proplists:get_value('password', Env, ""),
   {ok, Post}     = be2bill_lib:compute_post(Data, Password),
   gen_fsm:send_all_state_event(self(), 'try'),
	{reply, ok, main, StateData#state{post=Post}}.

main({timeout, _, _}, _From, StateData) ->
	{reply, ignored, main, StateData};
main(_Event, _From, StateData) ->
	{reply, ignored, main, StateData}.

backup({timeout, _, _}, _From, StateData) ->
	{reply, ignored, backup, StateData};
backup(_Event, _From, StateData) ->
	{reply, ignored, backup, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

%% Receive info from others that a server is ok
handle_info({'alive', Server }, StateName, StateData) ->
	{next_state, StateName, StateData#state{next = Server}}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
