%%%-----------------------------------------------------------------------------
%%% File:      be2bill_netlib.erl
%%% @author    Eric Pailleau <be2bill@crownedgrouse.com>
%%% @copyright 2016 crownedgrouse.com
%%% @doc  
%%% Networking library for Be2bill API
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
-module(be2bill_netlib).
-author("Eric Pailleau <be2bill@crownedgrouse.com>").

-export([pool2ips/1]).
-export([get_ips_from_cidr/1, get_ips_from_cidr/2]).

%%------------------------------------------------------------------------------
%% @doc Convert IP pool to IP list
%% @end
%%------------------------------------------------------------------------------
-spec pool2ips(list()) -> list().

pool2ips(Pools) when is_list(Pools) ->  
      lists:flatten(lists:map(fun(X) -> get_ips_from_cidr(X) end, Pools)).

%%------------------------------------------------------------------------------
%% @doc Convert IPv4/CIDR string to IPv4 list
%%      @todo : TODO IPv6 
%% @end
%%------------------------------------------------------------------------------
-spec get_ips_from_cidr(list()) -> list().

get_ips_from_cidr(X) when is_list(X) -> 
               [Ip, Cidr] = string:tokens(X, "/"),               
               case inet:parse_address(Ip) of
                  {ok, IpTuple}   -> get_ips_from_cidr(IpTuple, list_to_integer(Cidr)) ;
                  {error, einval} -> []
               end.

%%------------------------------------------------------------------------------
%% @doc Convert IPv4 start and CIDR to host list
%%      @todo : TODO IPv6 
%% @end
%%------------------------------------------------------------------------------
-spec get_ips_from_cidr(tuple(), non_neg_integer()) -> list().

get_ips_from_cidr(Ip, Cidr) when (Cidr < 33 ),(Cidr > 0 ) -> 
               Start = ip_to_int(Ip),
               NbHost = round(cidr2hostnumber(Cidr)),
               lists:map(fun(X) -> int_to_ip(Start + X) end, lists:seq(1, NbHost)).

%%------------------------------------------------------------------------------
%% @doc Convert CIDR to bits for 2 power 
%%      @todo : TODO IPv6 
%% @end
%%------------------------------------------------------------------------------
-spec cidr2bits(non_neg_integer()) -> non_neg_integer().

cidr2bits(Cidr) when is_integer(Cidr),(Cidr < 33 ),(Cidr > 0 ) -> 
      32 - Cidr .

%%------------------------------------------------------------------------------
%% @doc Convert Cidr to host number
%% @end
%%------------------------------------------------------------------------------
-spec cidr2hostnumber(non_neg_integer()) -> non_neg_integer().

cidr2hostnumber(Cidr) when is_integer(Cidr),(Cidr < 33 ),(Cidr > 0 ) -> 
      (math:pow(2, cidr2bits(Cidr))) - 2 .

%%------------------------------------------------------------------------------
%% @doc Convert Erlang IPv4 address tuple to integer 
%%      @todo : TODO IPv6 
%% @end
%%------------------------------------------------------------------------------
-spec ip_to_int(tuple()) ->  non_neg_integer().

ip_to_int({A,B,C,D}) -> 
      (A*16777216)+(B*65536)+(C*256)+(D).

%%------------------------------------------------------------------------------
%% @doc Convert integer to IPv4 address
%%      @todo : TODO IPv6 
%% @end
%%------------------------------------------------------------------------------
-spec int_to_ip(non_neg_integer()) -> tuple().

int_to_ip(Ip) when is_integer(Ip),(Ip > -1) -> 
      {Ip bsr 24, (Ip band 16711680) bsr 16, (Ip band 65280) bsr 8, Ip band 255}.


