%%******************************************************************************
%% Record definition for options in buildPaymentFormButton 
-record('buildAuthorizationFormButtonOpts',
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

-record('buildAuthorizationFormButton',
{'VERSION'= "2.0" :: string()
,'AMOUNT' 	      :: non_neg_integer() | list()
,'ORDERID'        :: string()     % uniq order id on merchand website
,'CLIENTIDENT'	   :: string()     % uniq client id on merchand website
,'DESCRIPTION' 	:: string()     % string 510 maximum
,'htmlOpts'       :: undefined | string()     
,'opts'           :: undefined | #buildAuthorizationFormButtonOpts{}   
}).



%%******************************************************************************
%% Record definition for options in oneClickAuthorization
-record('oneClickAuthorizationOpts',
{'AMOUNTS' 	                   :: list()
,'CLIENTADDRESS'               :: string()	  % string 510 max
,'CLIENTDOB' 	                :: string()     % YYYY-MM-DD
,'EXTRADATA' 	                :: string()     % string 255 max
,'CREATEALIAS' 	             :: 'yes' | 'no'  
,'DISPLAYCREATEALIAS'          :: 'yes' | 'no' 	
,'CARDCVV'  	                :: string()	% Visa - Mastercard : string 3     Amex : string 4
,'SHIPTOFIRSTNAME'             :: string() 	% string 15 max
,'SHIPTOLASTNAME'              :: string() 	% string 30 max
,'SHIPTOADDRESS'               :: string()	% string 50 max
,'SHIPTOCOUNTRY'               :: string()	% string 2
,'SHIPTOPOSTALCODE'            :: string()	% string 9 max
,'SHIPTOPHONE' 	             :: string()   % string 32 max [int]
,'BILLINGFIRSTNAME'            :: string()	% string 15 max
,'BILLINGLASTNAME'             :: string()	% string 30 max
,'BILLINGADDRESS'              :: string()	% string 50 max
,'BILLINGCOUNTRY'              :: string()	% string 2
,'BILLINGPOSTALCODE'           :: string() 	% string 9 max
,'BILLINGPHONE' 	             :: string()   % string 32 max [int]
,'SHIPTOCITY'  	             :: string()	% string 255 max
,'BILLINGCITY' 	 	          :: string()   % string 255 max
}).

-record('oneClickAuthorization',
{'VERSION'= "2.0"              :: string()
,'ALIAS'                       :: string()
,'AMOUNT'                      :: non_neg_integer() | list()
,'ORDERID'                     :: string()
,'CLIENTIDENT'                 :: string()
,'CLIENTEMAIL'                 :: string()
,'CLIENTIP'                    :: string()
,'DESCRIPTION'                 :: string()
,'CLIENTUSERAGENT'             :: string()
,'opts'                        :: undefined | #oneClickAuthorizationOpts{}
}).


%%******************************************************************************
%% Record definition for options in 
-record('subscriptionAuthorizationOpts',
{'CLIENTADDRESS'               :: string()   % string 510 max
,'CLIENTDOB' 	                :: string()   % YYYY-MM-DD
,'EXTRADATA'                   :: string()   % string 255 max
,'SHIPTOFIRSTNAME'             :: string() 	% string 15 max
,'SHIPTOLASTNAME'              :: string() 	% string 30 max
,'SHIPTOADDRESS'               :: string()	% string 50 max
,'SHIPTOCOUNTRY'               :: string()	% string 2
,'SHIPTOPOSTALCODE'            :: string()	% string 9 max
,'SHIPTOPHONE' 	             :: string()   % string 32 max [int]
,'BILLINGFIRSTNAME'            :: string() 	% string 15 max
,'BILLINGLASTNAME' 	          :: string()   % string 30 max
,'BILLINGADDRESS' 	          :: string()   % string 50 max
,'BILLINGCOUNTRY' 	          :: string()   % string 2
,'BILLINGPOSTALCODE'           :: string() 	% string 9 max
,'BILLINGPHONE'                :: string() 	% string 32 max [int]
,'SHIPTOCITY'                  :: string() 	% string 255 max
,'BILLINGCITY'                 :: string() 	% string 255 max
}).

-record('subscriptionAuthorization',
{'VERSION'= "2.0"              :: string()
,'ALIAS'                       :: string()
,'AMOUNT'                      :: non_neg_integer() | list()
,'ORDERID'                     :: string()
,'CLIENTIDENT'                 :: string()
,'CLIENTEMAIL'                 :: string()
,'CLIENTIP'                    :: string()
,'DESCRIPTION'                 :: string()
,'CLIENTUSERAGENT'             :: string()
,'opts'                        :: undefined | #subscriptionAuthorizationOpts{}
}).


%%******************************************************************************
%% Record definition for options in 
-record('authorizationOpts',
{'CLIENTADDRESS'              :: string() 	   % string 510 max
,'CLIENTDOB'                  :: string()  	   % YYYY-MM-DD
,'LANGUAGE' 	               :: 'FR' | 'EN' | 'DE' | 'ES' | 'IT' | 'NL' | 'CN' | 'RU' | 'PT' | 'CZ'
,'EXTRADATA'                  :: string()	      % string 255 max
,'CREATEALIAS'                :: 'yes' | 'no'	
,'3DSECURE' 	               :: 'yes' | 'no'  
,'SHIPTOFIRSTNAME'            :: string()	      % string 15 max
,'SHIPTOLASTNAME'             :: string()	      % string 30 max
,'SHIPTOADDRESS' 	            :: string()       % string 50 max
,'SHIPTOCOUNTRY'              :: string()	      % string 2
,'SHIPTOPOSTALCODE' 	         :: string()       % string 9 max
,'SHIPTOPHONE'  	            :: string()       % string 32 max [int]
,'BILLINGFIRSTNAME' 	         :: string() 	   % tring 15 max
,'BILLINGLASTNAME'  	         :: string()	      % string 30 max
,'BILLINGADDRESS'  	         :: string()     	% string 50 max
,'BILLINGCOUNTRY' 	 	      :: string()       % string 2
,'BILLINGPOSTALCODE'  	      :: string()	      % string 9 max
,'BILLINGPHONE' 	 	         :: string()       % string 32 max [int]
,'SHIPTOCITY'  	            :: string()	      % string 255 max
,'BILLINGCITY'  	            :: string() 	   % string 255 max
}).

-record('authorization',
{'VERSION'= "2.0"             :: string()
,'CARDPAN'                    :: non_neg_integer()
,'CARDDATE'                   :: string()
,'CARDCRYPTOGRAM'             :: non_neg_integer()
,'CARDFULLNAME'               :: string()
,'AMOUNT'                     :: non_neg_integer() | list()
,'ORDERID'                    :: string()
,'CLIENTIDENT'                :: string()
,'CLIENTEMAIL'                :: string()
,'CLIENTIP'                   :: string()
,'DESCRIPTION'                :: string()
,'CLIENTUSERAGENT'            :: string()
,'htmlOpts'                   :: string()
,'opts'                       :: undefined | #authorizationOpts{}
}).


