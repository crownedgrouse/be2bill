%%%-----------------------------------------------------------------------------
%%% File:      be2bill_simplesup.erl
%%% @author    Eric Pailleau <be2bill@crownedgrouse.com>
%%% @copyright 2016 crownedgrouse.com
%%% @doc  
%%% Erlang application http fsm supervisor for Be2bill API
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
-module(be2bill_simplesup).
-author("Eric Pailleau <be2bill@crownedgrouse.com>").
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link([Mode]) ->
    supervisor:start_link(be2bill_simple_sup, Mode).

init(Mode) ->

    WorkerSpec = {be2bill_fsm, {be2bill_fsm, start_link, Mode}
                 ,temporary
                 ,2000
                 ,worker
                 ,[be2bill_fsm]},
    {ok,{{simple_one_for_one, 0, 1}, [WorkerSpec]}}.

