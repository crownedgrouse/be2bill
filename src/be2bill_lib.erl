%%%-----------------------------------------------------------------------------
%%% File:      be2bill_lib.erl
%%% @author    Eric Pailleau <be2bill@crownedgrouse.com>
%%% @copyright 2016 crownedgrouse.com
%%% @doc  
%%% Library of Erlang application for Be2bill API
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
-module(be2bill_lib).
-author("Eric Pailleau <be2bill@crownedgrouse.com>").

-include("be2bill_defs.hrl").

-define(COMMON_CHECK(R, N),
   %% Note for coders : record_info is not a proper function. 
   %% It only exists during compilation, which means that it cannot take variable arguments.
    L =  tuple_to_list(R),
	 {C, _} = lists:mapfoldl(fun(X, A) -> {{X, check_spec(X), lists:nth(A, L)}, A + 1 }  end
                           , 2
                           , record_info(fields, N)),
    check_record(C)
).

-export([check_record/1, compute_post/2]).

%%------------------------------------------------------------------------------
%% @doc TODO
%% @end
%%------------------------------------------------------------------------------
compute_post(D, Passwd) -> 
             FD = lists:flatten(D),
             % Compute hashon data
             H = {'HASH', compute(FD, Passwd)},             
             % Order data with HASH entry
             SD = lists:sort(FD ++ [H]),
             % Compose the POST string
             {E, _}= lists:mapfoldl(fun({K, V}, Acc) -> Z = atom_to_list(K)++ "=" ++ to_list(V) ,{Z, Acc} end, [], SD),
             Post = string:join(E, "&"),
             {ok, Post}.

%%------------------------------------------------------------------------------
%% @doc Return SHA256 string of input
%% @end
%%------------------------------------------------------------------------------
-spec sha256_string( any() ) -> list(). 

sha256_string(S) -> hash_string(crypto:hash(sha256,S)).

%%------------------------------------------------------------------------------
%% @doc Let binary hash being string
%% @end
%%------------------------------------------------------------------------------
-spec hash_string( binary() ) -> list().

hash_string(X) -> lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(X)]).


%%------------------------------------------------------------------------------
%% @doc Compute hash
%% All the Be2bill parameters that compose the request, alphabetically ordered (A - Z),
%% formatted FIELD=value, immediately followed by another occurence of the Be2bill secret password
%% This string is then hashed with the SHA256 algorithm.
%% @end
%%------------------------------------------------------------------------------

compute(Data, Password) -> 
             % Order data
             SD = lists:sort(Data),
             {E, _}= lists:mapfoldl(fun({K, V}, Acc) -> Z = atom_to_list(K)++ "=" ++ to_list(V) ,{Z, Acc} end, [], SD),
             % join entries with password
             X = string:join(E, Password),
				 sha256_string(Password ++ X ++ Password).

%%------------------------------------------------------------------------------
%% @doc Convert data to list (string)
%% @end
%%------------------------------------------------------------------------------
to_list(V) when is_atom(V)    -> atom_to_list(V) ;
to_list(V) when is_integer(V) -> integer_to_list(V) ;
to_list(V) when is_float(V)   -> float_to_list(V) ;
to_list(V) -> V.

%%------------------------------------------------------------------------------
%% @doc Check a hash received in a notification/redirection URL
%% @end
%%------------------------------------------------------------------------------

checkHash(Params) -> ok. 

%%------------------------------------------------------------------------------
%% @doc Test if it is a valid URL
%% @end
%%------------------------------------------------------------------------------
-spec isHttpUrl( list() ) -> boolean().

isHttpUrl(Url) -> case httpc:parse(Url) of
						{ok, _}    -> true ;
						{error, _} -> false 
				  end.

%%------------------------------------------------------------------------------
%% @doc Test if it is a valid email
%% @end
%%------------------------------------------------------------------------------
-spec isMail( list() ) -> boolean().

isMail(Mail) -> true.

%%------------------------------------------------------------------------------
%% @doc Handle DATE or STARTDATE/ENDDATE parameters for export methods
%% @end
%%------------------------------------------------------------------------------
-spec getDateOrDateRangeParameter(tuple()) -> list(). 

getDateOrDateRangeParameter({D1, D2}) when (D1 < D2)  -> [{'STARDATE', D1}, {'ENDDATE', D2}];

getDateOrDateRangeParameter({D1, D2}) when (D1 > D2)  -> [{'STARDATE', D2}, {'ENDDATE', D1}];

getDateOrDateRangeParameter({D1, D2}) when (D1 == D2) -> [{'DATE', D1}];

getDateOrDateRangeParameter(D) -> [{'DATE', D}].
   
%%------------------------------------------------------------------------------
%% @doc Handle amount or ntimes amounts parameter
%% @end
%%------------------------------------------------------------------------------
-spec amountOrAmounts( list() | float() | integer()) -> list().

amountOrAmounts(Amount) when is_list(Amount) -> [{'AMOUNTS', Amount}] ;

amountOrAmounts(Amount) -> [{'AMOUNT', Amount}].

%%------------------------------------------------------------------------------
%% @doc Check record values when type is not sufficient
%% @end
%%------------------------------------------------------------------------------
-spec check_record(tuple() | list()) -> {ok, list()} | {error, list()}.

check_record(R) when is_record(R, 'authorization')  ->
    ?COMMON_CHECK(R, 'authorization');
check_record(R) when is_record(R, 'authorizationOpts')  ->
    ?COMMON_CHECK(R, 'authorizationOpts');
check_record(R) when is_record(R, 'buildAuthorizationFormButton')  ->
    ?COMMON_CHECK(R, 'buildAuthorizationFormButton');
check_record(R) when is_record(R, 'buildAuthorizationFormButtonOpts')  ->
    ?COMMON_CHECK(R, 'buildAuthorizationFormButtonOpts');
check_record(R) when is_record(R, 'buildPaymentFormButton')  ->
    ?COMMON_CHECK(R, 'buildPaymentFormButton');
check_record(R) when is_record(R, 'buildPaymentFormButtonOpts')  ->
    ?COMMON_CHECK(R, 'buildPaymentFormButtonOpts');
check_record(R) when is_record(R, 'capture')  ->
    ?COMMON_CHECK(R, 'capture');
check_record(R) when is_record(R, 'captureOpts')  ->
    ?COMMON_CHECK(R, 'captureOpts');
check_record(R) when is_record(R, 'exportChargebacks')  ->
    ?COMMON_CHECK(R, 'exportChargebacks');
check_record(R) when is_record(R, 'exportChargebacksOpts')  ->
    ?COMMON_CHECK(R, 'exportChargebacksOpts');
check_record(R) when is_record(R, 'exportReconciledTransactions')  ->
    ?COMMON_CHECK(R, 'exportReconciledTransactions');
check_record(R) when is_record(R, 'exportReconciledTransactionsOpts')  ->
    ?COMMON_CHECK(R, 'exportReconciledTransactionsOpts');
check_record(R) when is_record(R, 'exportReconciliation')  ->
    ?COMMON_CHECK(R, 'exportReconciliation');
check_record(R) when is_record(R, 'exportReconciliationOpts')  ->
    ?COMMON_CHECK(R, 'exportReconciliationOpts');
check_record(R) when is_record(R, 'exportTransactions')  ->
    ?COMMON_CHECK(R, 'exportTransactions');
check_record(R) when is_record(R, 'exportTransactionsOpts')  ->
    ?COMMON_CHECK(R, 'exportTransactionsOpts');
check_record(R) when is_record(R, 'getTransactionsByOrderId')  ->
    ?COMMON_CHECK(R, 'getTransactionsByOrderId');
check_record(R) when is_record(R, 'getTransactionsByOrderIdOpts')  ->
    ?COMMON_CHECK(R, 'getTransactionsByOrderIdOpts');
check_record(R) when is_record(R, 'getTransactionsByTransactionId')  ->
    ?COMMON_CHECK(R, 'getTransactionsByTransactionId');
check_record(R) when is_record(R, 'getTransactionsByTransactionIdOpts')  ->
    ?COMMON_CHECK(R, 'getTransactionsByTransactionIdOpts');
check_record(R) when is_record(R, 'oneClickAuthorization')  ->
    ?COMMON_CHECK(R, 'oneClickAuthorization');
check_record(R) when is_record(R, 'oneClickAuthorizationOpts')  ->
    ?COMMON_CHECK(R, 'oneClickAuthorizationOpts');
check_record(R) when is_record(R, 'oneClickPayment')  ->
    ?COMMON_CHECK(R, 'oneClickPayment');
check_record(R) when is_record(R, 'oneClickPaymentOpts')  ->
    ?COMMON_CHECK(R, 'oneClickPaymentOpts');
check_record(R) when is_record(R, 'payment')  ->
    ?COMMON_CHECK(R, 'payment');
check_record(R) when is_record(R, 'paymentOpts')  ->
    ?COMMON_CHECK(R, 'paymentOpts');
check_record(R) when is_record(R, 'redirectForPayment')  ->
    ?COMMON_CHECK(R, 'redirectForPayment');
check_record(R) when is_record(R, 'redirectForPaymentOpts')  ->
    ?COMMON_CHECK(R, 'redirectForPaymentOpts');
check_record(R) when is_record(R, 'refund')  ->
    ?COMMON_CHECK(R, 'refund');
check_record(R) when is_record(R, 'refundOpts')  ->
    ?COMMON_CHECK(R, 'refundOpts');
check_record(R) when is_record(R, 'stopNTimes')  ->
    ?COMMON_CHECK(R, 'stopNTimes');
check_record(R) when is_record(R, 'subscriptionAuthorization')  ->
    ?COMMON_CHECK(R, 'subscriptionAuthorization');
check_record(R) when is_record(R, 'subscriptionAuthorizationOpts')  ->
    ?COMMON_CHECK(R, 'subscriptionAuthorizationOpts');
check_record(R) when is_record(R, 'subscriptionPayment')  ->
    ?COMMON_CHECK(R, 'subscriptionPayment');
check_record(R) when is_record(R, 'subscriptionPaymentOpts')  ->
    ?COMMON_CHECK(R, 'subscriptionPaymentOpts');
% Effective check
check_record(C) when is_list(C) -> % For each field of the record, check value against field spec
    Raw = lists:map(fun({F, S, V}) -> 
            case V of  
               undefined -> [] ;
               _ ->  
                     case S of
                        error      -> {error, F} ; % Should not happen once project fully tested
                        recursive  -> case check_record(V) of
                                           {ok, Ok }    -> Ok ;
                                           {error, Err} -> Err
                                      end ;
                        skip       -> %io:format("OK  skip ~p ~p ~p ~n", [F, S, V]),
                                      {F, V} ; % Checked by record type
                        {ereg, RE} -> case re:run(V, RE) of
                                          {match, _} -> %io:format("OK  ereg ~p ~p ~p ~n", [F, S, V]), 
                                                        {F, V} ; 
                                          nomatch    -> %io:format("ERR eref ~p ~p ~p ~n", [F, S, V]),
                                                        {error, F}
                                      end
                     end
            end
         end, C),	
    % Check in Raw if some errors
    {Err, Ok} = lists:partition(fun({A, B}) -> ( A =:= error ) end, lists:flatten(Raw)),
	 case Err of
         [] -> {ok, Ok} ;
         _  -> {error, lists:map(fun({_, E}) -> E end, Err)}
    end.
					  
%%------------------------------------------------------------------------------
%% @doc Return specs to check data
%%     error          when unexpected spec is asked
%%     skip           when already checked at record state
%%     {ereg, ...}    when a regex is needed
%%     {lib, {M, F}}  when a function call is needed
%%     recursive      when value is another record to check
%%     todo           when no spec known TODO
%% @end
%%------------------------------------------------------------------------------
-spec check_spec( atom() ) -> error | skip | {ereg, string()} | {lib, tuple()} | recursive | todo .

check_spec('3DSECURE') -> 
	 skip ;
check_spec('3DSECUREDISPLAYMODE') -> 
	 skip ;
check_spec('AGEVERIFICATION') -> 
	 skip ;
check_spec('ALIAS') -> 
    {ereg, "^.{1,32}$"};
check_spec('ALIASMODE') -> 
	 skip ;
check_spec('AMOUNT') -> 
	 skip ;
check_spec('AMOUNTS') -> 
	 skip ;
check_spec('AVSPOSTALCODE') -> % TODO
    todo ;
check_spec('AVSSTREETNAME') -> % TODO
    todo ;
check_spec('AVSSTREETNUMBER') -> % TODO
    todo ;
check_spec('BANK') -> 
    {ereg, "^.{8,11}$"};
check_spec('BILLINGADDRESS') -> 
    {ereg, "^.{1,50}$"};
check_spec('BILLINGCITY') -> 
    {ereg, "^.{1,255}$"};
check_spec('BILLINGCOUNTRY') -> 
    {ereg, "^.{2}$"};
check_spec('BILLINGFIRSTNAME') -> 
    {ereg, "^.{1,15}$"};
check_spec('BILLINGLASTNAME') -> 
    {ereg, "^.{,30}$"};
check_spec('BILLINGMOBILEPHONE') -> 
    {ereg, "^[0-9]{1,32}$"};
check_spec('BILLINGPHONE') -> 
    {ereg, "^[0-9]{1,32}$"};
check_spec('BILLINGPOSTALCODE') -> 
    {ereg, "^.{1,9}$"};
check_spec('CALLBACKURL') -> 
    {lib, {be2bill_lib, check_url}};
check_spec('CARDCODE') -> 
    {ereg, "^.{13,19}$"};
check_spec('CARDCVV') -> 
    {ereg, "^.{3,4}$"};
check_spec('CARDFULLNAME') -> 
    {ereg, "^.{1,255}$"};
check_spec('CARDVALIDITYDATE') -> 
    {ereg, "^(0[1-9]|1[0-2])\-[0-9]{2}$"};
check_spec('DISCOUNT') -> 
    {ereg, "^[0-9]{1,2}|[0-9]{1,2}\.[0-9]{1,}$"};
check_spec('MERCHANTITEMID') -> % TODO
    todo ;
check_spec('NAME') -> % TODO
    todo ;
check_spec('PRICE') -> 
    skip ;
check_spec('QUANTITY') -> 
    skip ;
check_spec('TAX') -> 
    {ereg, "^[0-9]{1,2}|[0-9]{1,2}\.[0-9]{1,}$"};
check_spec('TOTALAMOUNT') -> 
    skip ;
check_spec('CLIENTADDRESS') -> 
    {ereg, "^.{1,510}$"};
check_spec('CLIENTDOB') -> 
    {ereg, "^[0-9]{4}\-(0[1-9]|1[0-2])\-(0[1-9]|1[0-9]|2[0-9]|3[0-1])$"};
check_spec('CLIENTEMAIL') -> 
    {ereg, "^.{1,255}$"};
check_spec('CLIENTGENDER') -> 
	 skip ;
check_spec('CLIENTIDENT') -> 
    {ereg, "^.{1,255}$"};
check_spec('CLIENTIP') -> 
    {ereg, "^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$"};
check_spec('CLIENTREFERRER') ->   
    {lib, {be2bill_lib, check_url}};
check_spec('CLIENTUSERAGENT') -> 
    {ereg, "^.{1,255}$"};
check_spec('COLUMNS') -> % TODO
    todo ;
check_spec('COMPRESSION') -> 
	 skip ;
check_spec('CREATEALIAS') -> 
	 skip ;
check_spec('DATE') -> 
    {ereg, "^[0-9]{4}\-(0[1-9]|1[0-2])\-(0[1-9]|1[0-9]|2[0-9]|3[0-1])|[0-9]{4}\-(0[1-9]|1[0-2])$"};
check_spec('DAY') -> 
    {ereg, "^[0-9]{4}\-(0[1-9]|1[0-2])\-(0[1-9]|1[0-9]|2[0-9]|3[0-1])$"};
check_spec('DESCRIPTION') -> 
    {ereg, "^.{1,510}$"};
check_spec('DESCRIPTOR') -> % TODO
    todo ;
check_spec('DISPLAYCREATEALIAS') -> 
	 skip ;
check_spec('EXTRADATA') -> 
    {ereg, "^.{0,255}$"};
check_spec('HASH') -> 
    {ereg, "^.{64}$"};
check_spec('HIDECARDFULLNAME') -> 
	 skip ;
check_spec('HIDECLIENTEMAIL') -> 
	 skip ;
check_spec('IBAN') -> 
    {ereg, "^.{34}$"};
check_spec('IDENTIFICATIONDOCID') -> % TODO
    todo ;
check_spec('IDENTIFICATIONDOCTYPE') -> % TODO
    todo ;
check_spec('IDENTIFIER') -> 
    {ereg, "^.{32}$"};
check_spec('LANGUAGE') -> 
	 skip ;
check_spec('MAILTO') -> % TODO
    todo ;
check_spec('METADATA') -> 
    {ereg, "^.{0,255}$"};
check_spec('OPERATIONTYPE') -> 
	 skip ;
check_spec('ORDERID') -> 
    {ereg, "^.{1,40}$"};
check_spec('REDIRECTPOSTPARAMS') -> % TODO
    todo ;
check_spec('REDIRECTURL') -> 
    {lib, {be2bill_lib, check_url}};
check_spec('SCHEDULEID') -> 
    {ereg, "^.{1,32}$"};
check_spec('SCOPE') -> % TODO
    todo ;
check_spec('SHIPTOADDRESS') -> 
    {ereg, "^.{1,50}$"};
check_spec('SHIPTOCITY') -> 
    {ereg, "^.{1,255}$"};
check_spec('SHIPTOCOUNTRY') -> 
    {ereg, ".{2}$"};
check_spec('SHIPTOFIRSTNAME') -> 
    {ereg, "^.{1,15}"};
check_spec('SHIPTOLASTNAME') -> 
    {ereg, "^.{1,30}$"};
check_spec('SHIPTOPHONE') -> 
    {ereg, "^[0-9]{1,32}$"};
check_spec('SHIPTOPOSTALCODE') -> 
    {ereg, "^.{1,9}$"};
check_spec('TIMEZONE') -> % TODO
    todo ;
check_spec('TRANSACTIONEXPIRATIONDATE') -> 
    {ereg, "^[0-9]{4}\-(0[1-9]|1[0-2])\-(0[1-9]|1[0-9]|2[0-9]|3[0-1])\ (0[0-9]|1[0-9]|2[0-3])\:[0-5][0-9]\:0-5][0-9]$"};
check_spec('TRANSACTIONID') -> 
    {ereg, "^.{1,32}$"};
check_spec('USETEMPLATE') -> 
	 skip ;
check_spec('VERSION') -> 
	 skip ;
check_spec('VME') -> % TODO
	 todo ;
check_spec('opts') ->
    recursive ;
check_spec('htmlOpts') ->
    recursive ;
check_spec(_) -> error.

