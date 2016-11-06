

%%******************************************************************************
%% Record definition for options in buildPaymentFormButton 
-record('buildPaymentFormButtonOpts',
{'CLIENTEMAIL' 	           :: string()            % string 255 max
,'CLIENTADDRESS' 	           :: string()            % string 510 max
,'CLIENTDOB'                 :: string()            % YYYY-MM-DD
,'LANGUAGE'                  :: 'FR' | 'EN' | 'DE' | 'ES' | 'IT' | 'NL' | 'CN' | 'RU' | 'PT' | 'CZ'
,'EXTRADATA'                 :: string()            % string 255 max
,'CREATEALIAS'               :: 'yes' | 'no'        
,'DISPLAYCREATEALIAS'        :: 'yes' | 'no'             
,'3DSECURE'                  :: 'yes' | 'no'        
,'3DSECUREDISPLAYMODE'       :: 'main' | 'popup' | 'top' | 'raw'
,'CARDFULLNAME'              :: string()            % string 255 max
,'HIDECLIENTEMAIL'           :: 'yes' | 'no' 
,'HIDECARDFULLNAME'          :: 'yes' | 'no'        
,'VME'                                              % 
,'SHIPTOFIRSTNAME'           :: string()            % string 15 max
,'SHIPTOLASTNAME'            :: string()            % string 30 max
,'SHIPTOADDRESS'             :: string()            % string 50 max
,'SHIPTOCOUNTRY'             :: string()            % string 2
,'SHIPTOPOSTALCODE'          :: string()            % string 9 max
,'SHIPTOPHONE'               :: string()            % string 32 max [int]
,'BILLINGFIRSTNAME'          :: string()            % string 15 max
,'BILLINGLASTNAME'           :: string()            % string 30 max
,'BILLINGADDRESS'            :: string()            % string 50 max
,'BILLINGCOUNTRY'            :: string()            % string 2
,'BILLINGPOSTALCODE'         :: string()            % string 9 max
,'BILLINGPHONE'              :: string()            % string 32 max [int]
,'TIMEZONE'                                         % default : UTC
,'TRANSACTIONEXPIRATIONDATE' :: string()            % YYYY-MM-DD hh:mm:ss. 
,'SHIPTOCITY'                :: string()            % string 255 max
,'BILLINGCITY'               :: string()            % string 255 max
}).

-record('buildPaymentFormButton',
{'AMOUNT' 	:: non_neg_integer() | list()
,'ORDERID'  :: string()           % uniq number on merchand website
,'CLIENTID'	:: string()           % string 255 maximum
,'DESCRIPTION' 	:: string()     % string 510 maximum
,'htmlOpts' :: undefined | string()     
,'opts'     :: undefined | #buildPaymentFormButtonOpts{}   	
}).

%%******************************************************************************
%% Record definition for options in capture
-record('captureOpts',
{'AMOUNT'                    :: non_neg_integer() 	                            
,'EXTRADATA' 	             :: string()            % string 255 max
}).

-record('capture',
{'TRANSACTIONID' :: string()  % Non empty string
,'ORDERID'       :: string()  % Non empty string
,'DESCRIPTION'   :: string()  % Optionally empty string < 511 chars
,'opts'          :: undefined | #captureOpts{}   % Optional record
}).

%%******************************************************************************
%% Record definition for options in refund
-record('refundOpts',
{'AMOUNT'            :: non_neg_integer() 	                                         
,'EXTRADATA' 	      :: string()                 % string 255 max
}).

-record('refund',
{'TRANSACTIONID' :: string()                     % Non empty string
,'ORDERID'       :: string()                     % Non empty string
,'DESCRIPTION'   :: string()                     % Optionally empty string < 511 chars
,'opts'          :: undefined | #captureOpts{}   % Optional record
}).

%%******************************************************************************
%% Record definition for options in oneClickPayment
-record('oneClickPaymentOpts',
{'AMOUNTS'  	                :: list()
,'CLIENTADDRESS'	             :: string() 	% string 510 max
,'CLIENTDOB'	                :: string() 	% YYYY-MM-DD
,'EXTRADATA'	                :: string() 	% string 255 max
,'CREATEALIAS'                 :: 'yes' | 'no'	
,'DISPLAYCREATEALIAS'          :: 'yes' | 'no' 	
,'CARDCVV'                     :: string()  	% Visa - Mastercard : string 3     Amex : string 4
,'SHIPTOFIRSTNAME'             :: string() 	% string 15 max
,'SHIPTOLASTNAME'              :: string() 	% string 30 max
,'SHIPTOADDRESS'               :: string()   % string 50 max
,'SHIPTOCOUNTRY'               :: string() 	% string 2
,'SHIPTOPOSTALCODE'            :: string()	% string 9 max
,'SHIPTOPHONE' 	             :: string()   % string 32 max [int]
,'BILLINGFIRSTNAME'            :: string() 	% string 15 max
,'BILLINGLASTNAME'             :: string()   % string 30 max
,'BILLINGADDRESS'              :: string()	% string 50 max
,'BILLINGCOUNTRY' 	          :: string()   % string 2
,'BILLINGPOSTALCODE' 	       :: string()   % string 9 max
,'BILLINGPHONE' 	 	          :: string()   % string 32 max [int]
,'SHIPTOCITY'  	 	          :: string()	% string 255 max
,'BILLINGCITY'  	 	          :: string()   % string 255 max
}).

-record('oneClickPayment',
{'ALIAS'              :: string()
,'AMOUNT'             :: non_neg_integer() | list()
,'ORDERID'            :: string()
,'CLIENTID'           :: string()
,'CLIENTEMAIL'        :: string()
,'CLIENTIP'           :: string()
,'DESCRIPTION'        :: string()
,'CLIENTUSERAGENT'    :: string()
,'opts'               :: undefined | #oneClickPaymentOpts{}
}).


%%******************************************************************************
%% Record definition for options in subscriptionPayment
-record('subscriptionPaymentOpts',
{'CLIENTADDRESS' 	            :: string()  % string 510 max
,'CLIENTDOB' 	               :: string()  % YYYY-MM-DD
,'EXTRADATA' 	               :: string()  % string 255 max
,'SHIPTOFIRSTNAME'            :: string()	 % string 15 max
,'SHIPTOLASTNAME' 	         :: string()  % string 30 max
,'SHIPTOADDRESS' 	            :: string()  % string 50 max
,'SHIPTOCOUNTRY'              :: string()	 % string 2
,'SHIPTOPOSTALCODE'           :: string()	 % string 9 max
,'SHIPTOPHONE' 	            :: string()  % string 32 max [int]
,'BILLINGFIRSTNAME'           :: string()	 % string 15 max
,'BILLINGLASTNAME'            :: string()	 % string 30 max
,'BILLINGADDRESS' 	         :: string()  % string 50 max
,'BILLINGCOUNTRY' 	         :: string()  % string 2
,'BILLINGPOSTALCODE'          :: string()  % string 9 max
,'BILLINGPHONE' 	            :: string()  % string 32 max [int]
,'SHIPTOCITY' 	               :: string()  % string 255 max
,'BILLINGCITY' 	            :: string()  % string 255 max
}).

-record('subscriptionPayment',
{'ALIAS'                       :: string()
,'AMOUNT'                      :: non_neg_integer() | list()
,'ORDERID'                     :: string()
,'CLIENTID'                    :: string()
,'CLIENTEMAIL'                 :: string()
,'CLIENTIP'                    :: string()
,'DESCRIPTION'                 :: string()
,'CLIENTUSERAGENT'             :: string()
,'opts'                        :: undefined | #subscriptionPaymentOpts{}
}).

%%******************************************************************************
%% Record definition for options in 
-record('redirectForPaymentOpts',
{'CLIENTADDRESS'               :: string()	% string 510 max
,'CLIENTDOB' 	                :: string()   % YYYY-MM-DD
,'EXTRADATA' 	                :: string()   % string 255 max
,'SHIPTOFIRSTNAME'             :: string()	% string 15 max
,'SHIPTOLASTNAME'              :: string()	% string 30 max
,'SHIPTOADDRESS'               :: string()	% string 50 max
,'SHIPTOCOUNTRY'               :: string()	% string 2
,'SHIPTOPOSTALCODE'            :: string()   % string 9 max
,'SHIPTOPHONE' 	             :: string()   % string 32 max [int]
,'BILLINGFIRSTNAME'            :: string()   % string 15 max
,'BILLINGLASTNAME'             :: string()   % string 30 max
,'BILLINGADDRESS'              :: string()   % string 50 max
,'BILLINGCOUNTRY'              :: string()	% string 2
,'BILLINGPOSTALCODE'           :: string()   % string 9 max
,'BILLINGPHONE' 	             :: string()   % string 32 max [int]
,'SHIPTOCITY' 	                :: string()   % string 255 max
,'BILLINGCITY' 	             :: string()   % string 255 max
}).

-record('redirectForPayment',
{'ALIAS'                       :: string()
,'AMOUNT'                      :: non_neg_integer() | list()
,'ORDERID'                     :: string()
,'CLIENTID'                    :: string()
,'CLIENTEMAIL'                 :: string()
,'CLIENTIP'                    :: string()
,'DESCRIPTION'                 :: string()
,'CLIENTUSERAGENT'             :: string()
,'opts'                        :: undefined | #redirectForPaymentOpts{}
}).

%%******************************************************************************
%% Record definition for options in 
-record('paymentOpts',
{'CLIENTADDRESS'              :: string()	  % string 510 max
,'CLIENTDOB' 	               :: string()   % YYYY-MM-DD
,'LANGUAGE' 	               :: 'FR' | 'EN' | 'DE' | 'ES' | 'IT' | 'NL' | 'CN' | 'RU' | 'PT' | 'CZ' 
,'EXTRADATA' 	               :: string()   % string 255 max
,'CREATEALIAS'	               :: 'yes' | 'no' 
,'3DSECURE' 	               :: 'yes' | 'no' 
,'SHIPTOFIRSTNAME'            :: string()	% string 15 max
,'SHIPTOLASTNAME'             :: string()	% string 30 max
,'SHIPTOADDRESS'              :: string()	% string 50 max
,'SHIPTOCOUNTRY'              :: string()	% string 2
,'SHIPTOPOSTALCODE'           :: string()	% string 9 max
,'SHIPTOPHONE'                :: string() % string 32 max [int]
,'BILLINGFIRSTNAME'           :: string()	% string 15 max
,'BILLINGLASTNAME'            :: string()	% string 30 max
,'BILLINGADDRESS' 	         :: string() % string 50 max
,'BILLINGCOUNTRY' 	         :: string() % string 2
,'BILLINGPOSTALCODE'          :: string()	% string 9 max
,'BILLINGPHONE'               :: string()	% string 32 max [int]
,'SHIPTOCITY' 	               :: string() % string 255 max
,'BILLINGCITY' 	            :: string() % string 255 max
}).


-record('payment',
{'CARDPAN'                    :: non_neg_integer()
,'CARDDATE'                   :: string()
,'CARDCRYPTOGRAM'             :: non_neg_integer()
,'CARDFULLNAME'               :: string()
,'AMOUNT'                     :: non_neg_integer() | list()
,'ORDERID'                    :: string()
,'CLIENTID'                   :: string()
,'CLIENTEMAIL'                :: string()
,'CLIENTIP'                   :: string()
,'DESCRIPTION'                :: string()
,'CLIENTUSERAGENT'            :: string()
,'htmlOpts'                   :: string()
,'opts'                       :: undefined | #paymentOpts{}
}).

%%******************************************************************************
%% Record definition for stopNTimes
-record('stopNTimes',
{'SCHEDULEID'                       :: string()
}).

