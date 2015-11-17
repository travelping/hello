hello
=====

JSON-RPC API toolkit

Version 3.2.0 - 17 Nov 2015
---------------------------
* dnssd is optional now and disabled by default
* new server and client metrics

Version 3.1.0 - 17 Sep 2015
---------------------------
* Elixir wrappers for hello and hello_client
* On zmq first frame should be empty
* Fix Keep Alive over HTTP
* Reconnect client if no pong answer
* add possibility to configure client and server timeouts
* Happy hello logging

Version 3.0.0 - 15 Jul 2015
---------------------------
* mDNS support via Apple Bonjour
* hackney as a HTTP client
* rebar3/mix support
* JSON-RPC singnatire
* Bug fixes and performance improvement

Version 2.2.4 - 24 Feb 2015
---------------------------

* fix handling of optional parameters

Version 2.2.3 - 16 Oct 2014
---------------------------

* fix stateless handler registration race on ZMQ transport
* convert request log to lager

Version 2.2.2 - 08 Jul 2014
---------------------------

* update to be compatible with cowboy 0.9.0/0.10.0

Version 2.2.0 - 20 Jun 2014
---------------------------

* Add backward compatibility for optional not sent parameters
* Add backward compatibility for no-yang hello bulk methods
* Merge hello2 in hello:
    - Fixes for types in hello2_stateful_handler
    - Fix module prefix and add new callback record
    - enhance error returns
    - update to actual yang
    - fix start routine and cowboy dependencies
    - [hello] fix start routines and stop cowboy listener routines
    - fix module, state by start for statefull compatibility
    - move argument validation from hello internal to yang models
    - Fix multi endpoint support for http listener.
    - [hello2] Fix multiple namespace issues
    - [hello] tetrapak_deps added
    - yang dependency added
* Fix bug by supervising of cowboy listeners

Version 2.1.1 - xx Nov 2013
---------------------------

* Fixes multi endpoint support for ZMQ and HTTP listeners
* Allows deeply nested API namespaces
