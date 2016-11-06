# be2bill Erlang API #

## Preamble ##
This project offer the use of [be2bill](https://www.be2bill.com/en/) services 
in [Erlang](http://www.erlang.org/) applications, mainly for Erlang CMS 
or commercial websites.

## Note for non Erlangers ##
This API is trying to be as close as possible to the [PHP API](https://github.com/Be2bill/php-merchant-api) 
with the notable difference that Erlang is a [functional langage](https://en.wikipedia.org/wiki/Erlang_(programming_language)), 
and not an Object Oriented langage.

To do so, object are [records](http://erlang.org/doc/reference_manual/records.html), 
and class methods are callbacks in [gen_server](http://erlang.org/doc/design_principles/gen_server_concepts.html).

## Other differences ##
In order to benefit of Erlang power (high availability, fault tolerance, ...), 
this project is not only a client API, but a real [OTP application](http://erlang.org/doc/design_principles/applications.html#id80391) 
that handles all the [be2bill logic](https://developer.be2bill.com/platform) 
(http request retries, switch to backup environment on long time error, batch mode, etc...).

This let the Erlang coder to concentrate on only the business logic in 
its own code, with a "fire and forget" approach, and having the insurance that 
_almost_ no transactions (see security paragraph hereafter) will be lost.

This project propose also some extra things, like metrics which can be usefull 
in commercial website with lot of transactions.

## Security aspects ##
### Git ###
This project comes with a `.gitignore` file that avoid any commit of configuration
files potentially containing sensitive be2bill identifiers, either production or sandbox ones. 
*DO NOT MODIFY OR REMOVE IT* if you fork this project ! keep in mind that if you committed 
them once, they may be still tracked in the git history.
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

## Usage ##
See [Wiki]() .








