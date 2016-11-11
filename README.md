# be2bill Erlang API #

## Preamble ##
This project offer the use of [be2bill](https://www.be2bill.com/en/) services 
in [Erlang](http://www.erlang.org/) applications, mainly for Erlang CMS 
or commercial websites.

## Note for non Erlangers ##
This API is trying to be as close as possible to the [PHP API](https://github.com/Be2bill/php-merchant-api) 
with the notable difference that Erlang is a [functional langage](https://en.wikipedia.org/wiki/Erlang_(programming_language)), 
and not an Object Oriented langage.

To do so, objects are [records](http://erlang.org/doc/reference_manual/records.html), 
and class methods are callbacks in [gen_server](http://erlang.org/doc/design_principles/gen_server_concepts.html).

## Other differences ##
In order to benefit of Erlang power (high availability, fault tolerance, ...), 
this project is not only a client API, but a real [OTP application](http://erlang.org/doc/design_principles/applications.html#id80391) 
that handles all the [be2bill logic](https://developer.be2bill.com/platform) 
(http request retries, switch to backup environment on long time error, batch mode, etc...).

This let the Erlang coder to concentrate on only the business logic in 
its own code, with a "fire and forget" approach, and having the insurance that 
_almost_ no transactions will be lost (see security paragraph hereafter).

This project propose also some extra things, like metrics which can be usefull 
for commercial website dealing with lot of transactions.

## Security aspects ##
### Git ###
This project comes with a `.gitignore` file that avoid any commit of `.config` configuration
files potentially containing sensitive be2bill identifiers, either production or sandbox ones.
(But this does not protect againt `-f` git argument however !) 
*DO NOT MODIFY OR REMOVE IT* if you fork this project ! 
Keep in mind that if you committed them once, they may be still tracked in the git history.
Configuration examples are coming with a `.config.dist` extension : *Never edit them*. 
Simply copy them by removing `.dist` extension and add your credentials there.

### Runtime ###
Payment processus imply to manipulate some sensitive data.
To prevent theft of sensitive data, Erlang processes linked to Production environment
receive a [flag 'sensitive'](http://erlang.org/doc/man/erlang.html#process_flag-2)
at creation, and therefore cannot be traced or observed at runtime.
This is *not* the case for sandbox environment processes, for debugging purpose, 
you are aware. 

Only non sensitive requests are stored on disk until they are commited.
So in case of a brutal machine crash, all pending transactions, but sensitive ones,
will be hopefully resumed at next start. 
This is the only case which may lead to a transaction loose.
`be2bill` application being stopped will refuse new transactions, 
and try to wait for current transactions to be finished before stopping.

## Getting started ##
Procedure below let you quickly start to test in a sanbox environment.
This is assumed that you got your own credentials from be2bill in order to use a sanbox.

### Clone project ###
```
$> git clone https://github.com/crownedgrouse/be2bill.git
$> cd be2bill/

```

### Compiling project ###
For `erlang.mk` users :

```
$> make
$> make bootstrap-rel
```

For `rebar` users :

```
$> rebar compile
```

### Create a sandbox configuration ###

```
$> cd priv/
$> cp sys.config.dist sys.config
$> cp production.config.dist production.config
$> cp sandbox.config.dist  sandbox.config
$> mv *.config ../rel/
$> cd ../rel/

```
Then edit `sandbox.config` (at least) in order to write your credentials to access be2bill's sandbox servers. 
The same with `production.config` if you are ready to do so.

At same time other configuration parameters can be tuned, see [Configuration](https://github.com/crownedgrouse/be2bill/wiki/Configuration).

### Starting release ###
For `erlang.mk` users :
```
$> make run

--- snip ---
(all release start stuff)
--- snip ---

Erlang/OTP 19 [erts-8.1] [source] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.1  (abort with ^G)

(be2bill@127.0.0.1)1> whereis(production).
<0.198.0>
(be2bill@127.0.0.1)2> whereis(sandbox).   
<0.200.0>
```
As you can see, each environment is reachable as a name (`production` or `sandbox`).
Those processes are gen_server where you can perform requests by submitting records.

Note if you are not using release but an interactive shell, simply run : 
```
1> application:start(be2bill).
ok
```
but you will need to load the be2bill config by passing start argument to Erlang node, like : `erl -sname test -config priv/sys `

### Testing manually some transactions ###

Once into an Erlang shell of an Erlang node where be2bill application is running, we can do manual transactions.
To do so, you need to load record definitions that will help you to compose valid be2bill records.
This is achieved by importing record definition available in `include/` directory.

```
(be2bill@127.0.0.1)3> rr("lib/be2bill-X.Y.Z/include/be2bill_defs.hrl").
[authorization,authorizationOpts,
 buildAuthorizationFormButton,
 buildAuthorizationFormButtonOpts,buildPaymentFormButton,
 buildPaymentFormButtonOpts,capture,captureOpts,
 exportChargebacks,exportChargebacksOpts,
 exportReconciledTransactions,
 exportReconciledTransactionsOpts,exportReconciliation,
 exportReconciliationOpts,exportTransactions,
 exportTransactionsOpts,getTransactionsByOrderId,
 getTransactionsByOrderIdOpts,getTransactionsByTransactionId,
 getTransactionsByTransactionIdOpts,oneClickAuthorization,
 oneClickAuthorizationOpts,oneClickPayment,
 oneClickPaymentOpts,payment,paymentOpts,redirectForPayment,
 redirectForPaymentOpts,refund|...]
```
(Replace `X.Y.Z` by the be2bill release version you are running.)

Note that if all record definitions are not needed, you can load only sub-definitions :
`be2bill_authorization.hrl`, `be2bill_payment.hrl`, `be2bill_transaction.hrl` .

`be2bill_defs.hrl` is only existing to load these three sub-definitions.

Submit a request is then simple as :
```
(be2bill@127.0.0.1)4> A = #payment{'ORDERID'="000123", 'DESCRIPTION'="art_123456", 'AMOUNT' = 1000, 'CLIENTID'="client_123"}.   
#payment{'CARDPAN' = undefined,'CARDDATE' = undefined,
         'CARDCRYPTOGRAM' = undefined,'CARDFULLNAME' = undefined,
         'AMOUNT' = 1000,'ORDERID' = "000123",
         'CLIENTID' = "client_123",'CLIENTEMAIL' = undefined,
         'CLIENTIP' = undefined,'DESCRIPTION' = "art_123456",
         'CLIENTUSERAGENT' = undefined,htmlOpts = undefined,
         opts = undefined}
(be2bill@127.0.0.1)5> gen_server:call(sandbox, A).
ok
```
Same thing for production, except that calls have to be done on `production` gen_server.

## Going further ##
See [Wiki](https://github.com/crownedgrouse/be2bill/wiki) .








