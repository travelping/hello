hello
=====

JSON-RPC API toolkit

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
