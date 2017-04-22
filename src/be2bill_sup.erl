%%%-----------------------------------------------------------------------------
%%% File:      be2bill_sup.erl
%%% @author    Eric Pailleau <be2bill@crownedgrouse.com>
%%% @copyright 2016 crownedgrouse.com
%%% @doc  
%%% Erlang application supervisor for Be2bill API
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
-module(be2bill_sup).
-author("Eric Pailleau <be2bill@crownedgrouse.com>").
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
   SupFlags = {one_for_one, 1, 5},
   % Check what environment type have to be started
   Envs = application:get_env(be2bill, 'env'  , []),
   % Dev
	Devs = case lists:member('dev_env', Envs) of
				   false -> [];
					true  -> lists:map(fun(X) -> {X, {be2bill_srv, start_link, [X]}, 
                                             permanent, 
                                             brutal_kill, 
                                             worker, [be2bill_srv]}
                                  end, application:get_env(be2bill, 'dev_env'  , [])) 
			 end,
   % Prod
	Prods = case lists:member('prod_env', Envs) of
				   false -> [];
					true  -> lists:map(fun(X) -> {X, {be2bill_srv, start_link, [X]}, 
                                             permanent, 
                                             brutal_kill, 
                                             worker, [be2bill_srv]} 
                                  end, application:get_env(be2bill, 'prod_env'  , [])) 
			 end,
	
   % Start workers   
	Procs = Devs ++ Prods ,
	{ok, {SupFlags, Procs}}.


