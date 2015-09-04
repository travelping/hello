### Logging in hello

##### This document describes the status codes used in hello. Status codes should be seen in logging as a 6-digit number prepended to the actual log message. The first 3 digits describe an abstract logging class while the last 3 digits are aligned to http status codes and define the logging statement more precisely.

| Logging Class                             | ID  |
|-------------------------------------------|-----|
| Initialization (app)                      | 000 |
| Initialization (client)                   | 100 |
| Initialization (server)                   | 110 |
| Message passing (client)                  | 200 |
| Message passing (server)                  | 210 |
| Message creation and parsing (client)     | 300 |
| Message creation and parsing (server)     | 310 |
| Routing (server only)                     | 400 |
| Metrics                                   | 500 |
| Registry                                  | 600 |

##### The following list will describe each used status code. The message and explanation part are unique for the specific last 3 digits of the status code. For every possible message a UUID was generated which can be used to identify a message in journal files.

#### 200400

* ___Messages___:
  * [ec504cee59b245c0861b78ccd936b856] Request from hello client @CLIENT@ on method(s) @METHOD@ failed with reason @REASON@.
  * [cb4c75e200f74a279ca3373e37eb7f81] Hello client @CLIENT@ attempted to send binary request on method(s) @METHOD@ but failed with reason @REASON@.
* ___Explanation___: Hello client attempted to send a request but failed due to issues with encoding or transport specific reasons.
* ___Level___: info

#### 300400

* ___Messages___:
  * [d7a051ed5dab4b50afa67ca5fadd6077] Creation of request from hello client @Client@ failed for a call with reason @REASON@.
  * [b68c94425c1f40aca74ef33e13eccbfc] Hello client @CLIENT@ attempted to encode request on method(s) @METHOD@ but failed with reason @REASON@.
* ___Explanation___: Hello client attempted to create a request but failed due to protocol specific reasons.
* ___Level___: info

#### 300401

* ___Messages___:
  * [c2763c7aba514282a48a87dee13e7738] Hello client @CLIENT@ failed to decode response with reason @REASON@.
* ___Explanation___: Hello client attempted to decode a response but failed due to issues with the protocol specific creation process.
* ___Level___: info

#### 300202

* ___Messages___:
  * [d77a63a5b17c4f0499fc9360083aa7a1] Hello client @CLIENT@ attempted to encode request on method(s) @METHOD@ but ignored sending request.
* ___Explanation___: Hello client attempted to encode or decode a response but ignored further processing.
* ___Level___: debug


#### 100200

* ___Messages___:
  * [ff6a6c3ae0734a65b2845c39d223b249] Initializing hello client @CLIENT@ on @URL@ ...
  * [b31ad32a76a94d78b866b2e168f9f7d3] Hello client @CLIENT@ initialized successfully.
  * [c2ba31819c384d76966d6bdb0cf02250] Hello client @CLIENT@ initialized successfully with keep alive.
* ___Explanation___: Hello client initializing process. Protocol and transport state are initialized.
* ___Level___: debug

#### 100400

* ___Messages___:
  * [bd8b9bc7d0ef48a98f6286c779781985] Hello client @CLIENT@ is unable to initialize protocol because of reason @REASON@.
  * [a55b446c103646b2a84543a907c13378] Hello client @CLIENT@ unable to initialize transport because of reason @REASON@.
* ___Explanation___: Hello client protocol or transport handler returned an error reason @REASON@.
* ___Level___: info

#### 200200

* ___Messages___:
  * [e9f0397b5b814c3abd55f3a474815f6c] Hello client @CLIENT@ received notification.
  * [f819220efa73409fa896aaafc0c21d30] Hello client @CLIENT@ received single response.
  * [a953af5734984f39b1c26a3541a1f48f] Hello client @CLIENT@ received batch response.
  * [bfde238fb72f420996cb52eaeda474df] Hello client @CLIENT@ sent request on method(s) @METHOD@.
* ___Explanation___: The client sent or received a response on a request.
* ___Level___: debug

#### 200201

* ___Messages___:
  * [bc9ed80e61cb441c952422c649a3ff3e] Hello client @CLIENT@ received internal message.
  * [d330d615c9a84f9dadd4a327fa3ac2fe] Hello client @CLIENT@ sent PING request. Pinging server again in @TIME@ milliseconds.
  * [ef4efe6bb336491990b99cc1113adb85] Hello client @CLIENT@ received PONG response. Pinging server again in @TIME@ milliseconds.
* ___Explanation___: The client sent or received a message for e.g. informational purposes. This does affect functionality of the client, e.g. keep alive mechanisms or similar.
* ___Level___: debug

#### 200202

* ___Messages___:
  * [d406dffdf263429eac4e64b7c52f7276] Hello client @CLIENT@ received error notification from transport handler with reason @REASON@.
  * [a887733cce7949f5a1d041441ccdfdfc] Hello client @CLIENT@ ignored decoding binary response.
  * [e61cd5d46b344504aad275c1f4831f90] Hello client @CLIENT@ got response for non-existing request id @ID@.
* ___Explanation___: The client sent or received a message for e.g. informational purposes. This does not affect functionality of the client.
* ___Level___: debug

#### 200211

* ___Messages___:
  * [e6fa0b7a7ffb45f29a359eca6776b369] Hello ZeroMQ client attempted to establish connection to @URL@ returning @RESULT@.
* ___Explanation___: The client transport initlializes a connection.
* ___Level___: debug

#### 200410

* ___Messages___:
  * [d3cbc27eb6ab43f2b2dfc9114c122054] Hello http client received an error after executing a request to @URL@ with reason @REASON@.
* ___Explanation___: The client sent or received an errors. This does affect functionality of the client.
* ___Level___: info

#### 200401

* ___Messages___:
  * [cd035f8d88aa45cca84b67a4227c8cd6] Error in hello client @CLIENT@: There is no PONG answer on PING for @TIME@ msec. Connection will be reestablished.
* ___Explanation___: The client sent or received an error message for e.g. informational purposes. This does affect functionality of the client.
* ___Level___: debug

#### 210500

* ___Messages___:
  * [e2f93d4effd2479fa7a6291b2c9da1dd] Hello handler with callback @CALLBACK@ and service id @ID@ dismissed bad request on method(s) @METHOD@.
* ___Explanation___: Hello handler attempted to handle an invalid request possibly triggering to stop the handler, e.g. validation procedures noticed an invalid method name or similar.
* ___Level___: info

#### 210200

* ___Messages___:
  * [fc965b4e012648578c379fe0faae3461] Hello handler with callback @CALLBACK@ and service id @ID@ answered async request on method(s) @METHOD@.
  * [c457bf8eb8574927bb1285bc32c18aff] Hello handler with callback @CALLBACK@ and service id @ID@ answered synced request on method(s) @METHOD@ in @TIME@ ms.
* ___Explanation___: Hello handler successfully proceeded a request.
* ___Level___: debug

#### 210202

* ___Messages___:
  * [a7237645a763489591c1559b7ec586b8] Hello handler with callback module @CALLBACK@ and service id @ID@ got unknown async reply.
  * [b9ab6b39134542fdbf0fabd7a88e8a19] Hello handler with callback module @CALLBACK@ and service id @ID@ received unknown idle timeout message.
  * [da812da4d818443a872767687edd47f3] Hello handler with callback @CALLBACK@ and service id @ID@ received async request  on method(s) @METHOD@ in @TIME@ ms.
  * [b3543661a8b2472984874c9c7325fa81] Hello handler with callback @CALLBACK@ and service id @ID@ answered synced request on method(s) @METHOD@ and stopped with reason @REASON@ in @TIME@ ms.
  * [aeb8bd41d1ed47048e6ea6056cade3ac] Hello handler with callback @CALLBACK@ and service id @ID@ received async request on method(s) @METHOD@ and stopped with reason @REASON@ in @TIME@ ms.
  * [aee0c58903824a139d1b1df028c223d6] Hello Handler with callback @CALLBACK@ and service id @ID@ received async request on method(s) @METHOD@ and stopped with reason normal in @TIME@ ms.
* ___Explanation___: Hello handler noticed difficulties in proceeding a request. This has no impact in the functionality of the handler (besides intended stop mechanisms).
* ___Level___: debug

#### 210501

* ___Messages___:
  * [aabd551dcc334fe093954d19b4dcc445] Hello handler with callback module @CALLBACK@ and service id @ID@ is going to stop due to idle timeout.
* ___Explanation___: Hello handler received an internal error message leading to functional changes of the handler.
* ___Level___: info

#### 100410

* ___Messages___:
  * [a95801c548ad4f508f315334ff79f199] Hello http client invoked with invalid options. Terminated with reason @REASON@.
* ___Explanation___: Hello client transport could't be initialized.
* ___Level___: info

#### 400200

* ___Messages___:
  * [c68287edeaac43fe96f49161eb8f44ba] Handler for service @NAME@ and identifier @ID@ not found. Starting handler.
  * [d3496f96a1154dad96e5ec469dda164b] Found handler @HANDLER@ for service @NAME@ and identifier @ID@.
  * [cfe782643a4e49a581a94c2ae146e683] Hello router resolved namespace @NAMESPACE@ to hello handler @HANDLER@.
  * [eab6276590fc4d84996662f4c13a271a] Hello http client: DNS discovery service resolved path @PATH@ to host @IP@.
  * [d80498ec152342eba94827f005123d27] Hello ZeroMQ client: DNS discovery service resolved path @PATH@ to host @IP@.
  * [af5e3e79776642e4851709b58e7ea790] DNS discovery service registered app @APP@ with name @NAME@ on port @PORT@.
* ___Explanation___: Hello router successfully attempts to find a handler for a request.
* ___Level___: debug

#### 400202

* ___Messages___:
  * [a48b0a9d2aaa477494fade8ace87f20f] Hello https client received message @MSG@ from DNS discovery service.
  * [eafe96b96fb447c59ba19d8452ee16f8] Hello ZeroMQ client ignored message @MSG@ from DNS discovery service.
  * [e849ee8a79b2435394e81b51894a617c] Hello ZeroMQ listener ignored message @MSG@ from DNS discovery service.
  * [a3fcdc9a486247d1af5ea679d0f724e9] Hello registry received message @MSG@ from DNS service discovery.
* ___Explanation___: Some module ignored a message from the DNS-DS.
* ___Level___: debug

#### 400500

* ___Messages___:
  * [aee45db4a0a4465687588331edc8bde4] Hello router couldn't find service for a key @KEY@. Request is dismissed.
  * [eda0efbe7f6d490490e00ae4055629d2] Hello service @NAME@ not found.
* ___Explanation___: Hello handler can't resolve a lookup and will return method_not_found.
* ___Level___: info

#### 210510

* ___Messages___:
  * [aee45db4a0a4465687588331edc8bde4] Hello http listener received an error while streaming the response body.
  * [dea8dfd8818e432bb61a64ca88e35a9e] Hello ZeroMQ listener received bad message @MSG@ from @PID@.
* ___Explanation___: Hello handler transport noticed problems handling a request or sending back a response.
* ___Level___: info

#### 210511

* ___Messages___:
  * [c0ea6cda30b64f10be3db0ee5fdc22d1] Hello ZeroMQ listener was unable to bind on @URL@ because of reason @REASON@.
* ___Explanation___: Hello handler transport noticed problems in the initializing process.
* ___Level___: info

#### 500500

* ___Messages___:
  * [adcfa613af3d40538bddb1e5fd178139] Hello metrics received unknown subscription type @TYPE@.
  * [b5886b31a45d422bb02422aaf072797b] Hello registry received unknown key @KEY@ for register metric.
* ___Explanation___: Hello metrics noticed problems in processing metric information.
* ___Level___: debug

#### 600500

* ___Messages___:
  * [bb0ea6d2b9c74a78afa724889ceee0b5] Hello registry received @EXIT@ signal from monitored process @PID@ with reason @REASON@.
  * [da5cd3be4435462dafff47b1bbc34a62] Hello registry received @EXIT@ signal from monitored process @PID@ with reason @REASON@. Going to clean up associated processes @PIDS@.
  * [ab5f921ed6e7488293241dc27eaf102e] Hello registry attempted to register process @PID@ with key @KEY@, but process is not alive.
* ___Explanation___: Hello registry noticed problems in registering or monitoring processes.
* ___Level___: info

#### 600200

* ___Messages___:
  * [bd8ad7e2a04940e8bc4fd7417d2907aa] Hello registry is going to register process @PID@ with key @KEY@ and data @DATA@.
* ___Explanation___: Hello registry registering process.
* ___Level___: debug

#### 000400

* ___Messages___:
  * [a3b473cec1c841d0ada3232fafe8d081] Hello supervisor got invalid list @LIST@ of roles.
  * [be1432aa9c074facae66b9d6786de2e5] Hello supervisor got invalid role @ROLE@.
* ___Explanation___: Problems in application initialization.
* ___Level___: info

#### 100420

* ___Messages___:
  * [ab65930009164e7fb6419b615b4ae4b5] Hello client invoked with invalid JSONRPC version @VERSION@.
* ___Explanation___: Problems in client protocol initialization.
* ___Level___: info

#### 310400

* ___Messages___:
  * [bd81f08a8fc94502bcbd07975e952016] Hello proto unable to decode binary request with error @ERROR@.
  * [c796ed5e01f0455fb09c42e0f04198c1] Hello proto attempted to decode invalid notification object.
* ___Explanation___: Problems in decoding binary requests.
* ___Level___: info