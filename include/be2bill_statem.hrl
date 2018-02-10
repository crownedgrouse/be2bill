-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
-type ip6_address() :: {0..65535, 0..65535,
                        0..65535, 0..65535,
                        0..65535, 0..65535,
                        0..65535, 0..65535}.
-type ip_address() :: ip4_address() | ip6_address().

-record(state,
{next   = undefined :: undefined | ip_address() % the server to be tried next time
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
