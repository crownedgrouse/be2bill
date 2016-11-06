
%%******************************************************************************
%% Record definition for options in getTransactionsByOrderId
-record('getTransactionsByOrderIdOpts',
{'TIMEZONE' 	% Timezone / default value : UTC
}).

-record('getTransactionsByOrderId',
{'orderIDList'                 :: string() | list()
,'to'                          :: string()
,'compression'                 :: 'zip' | 'gzip'  % | 'bzip' % unsupported in Erlang
,'opts'                        :: undefined | #getTransactionsByOrderIdOpts{}          
}).

%%******************************************************************************
%% Record definition for options in getTransactionsByTransactionId
-record('getTransactionsByTransactionIdOpts',
{'TIMEZONE' 	% Timezone / default value : UTC
}).

-record('getTransactionsByTransactionId',
{'orderIDList'                 :: string() | list()
,'to'                          :: string()
,'compression'                 :: 'zip' | 'gzip'  % | 'bzip' % unsupported in Erlang
,'opts'                        :: undefined | #getTransactionsByTransactionIdOpts{}          
}).

%%******************************************************************************
%% Record definition for options in 
-record('exportTransactionsOpts',
{'TIMEZONE' 	% Timezone / default value : UTC
}).

-record('exportTransactions',
{'date'                        :: string()
,'to'                          :: string()
,'compression'                 :: 'zip' | 'gzip'  % | 'bzip' % unsupported in Erlang
,'opts'                        :: undefined | #exportTransactionsOpts{} 
}).

%%******************************************************************************
%% Record definition for options in 
-record('exportChargebacksOpts',
{'TIMEZONE' 	% Timezone / default value : UTC
}).

-record('exportChargebacks',
{'date'                        :: string()
,'to'                          :: string()
,'compression'                 :: 'zip' | 'gzip'  % | 'bzip' % unsupported in Erlang
,'opts'                        :: undefined | #exportChargebacksOpts{} 
}).

%%******************************************************************************
%% Record definition for options in 
-record('exportReconciliationOpts',
{'TIMEZONE' 	 % Timezone / default value : UTC
,'SCOPE' 	    % TODO Level of importance of the data to export
}).

-record('exportReconciliation',
{'date'                        :: string()
,'to'                          :: string()
,'compression'                 :: 'zip' | 'gzip'  % | 'bzip' % unsupported in Erlang
,'opts'                        :: undefined | #exportReconciliationOpts{} 
}).

%%******************************************************************************
%% Record definition for options in 
-record('exportReconciledTransactionsOpts',
{'TIMEZONE' 	 % Timezone / default value : UTC
,'SCOPE' 	    % Level of importance of the data to export
}).

-record('exportReconciledTransactions',
{'day'                         :: string()
,'to'                          :: string()
,'compression'                 :: 'zip' | 'gzip'  % | 'bzip' % unsupported in Erlang
,'opts'                        :: undefined | #exportReconciledTransactionsOpts{} 
}).

