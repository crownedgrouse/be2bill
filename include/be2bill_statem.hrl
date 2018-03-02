-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
-type ip6_address() :: {0..65535, 0..65535,
                        0..65535, 0..65535,
                        0..65535, 0..65535,
                        0..65535, 0..65535}.
-type ip_address() :: ip4_address() | ip6_address().

-record(state,
{id     = 0   :: non_neg_integer()              % Id of request
,next   = undefined :: undefined | ip_address() % the server to be tried next time
,server = undefined :: undefined | ip_address() % the last server being tried
,tries  = 0   :: non_neg_integer()              % current number of tries
,status = 0   :: non_neg_integer()              % Http code received
,path         :: string()                       % path part of url
,post         :: string()                       % data to post
,http_options = [] :: list()                    % Http options
,options      = [] :: list()                    % Options
,main         = [] :: list()
,backup       = [] :: list()
}).

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
