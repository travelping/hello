-define(LOGID99, {ffffffffffffffffffffffffffffffff, 999999}). %placeholder

%% since erlang atoms are quoted if they begin with a number
%% we just take UUIDs which begin with a letter. Hence we have
%% no quoting in journald (like other message IDs).

%MESSAGE IDs and STATUS CODEs for hello_client
-define(LOGID00, {ec504cee59b245c0861b78ccd936b856, 200400}).
-define(LOGID01, {d7a051ed5dab4b50afa67ca5fadd6077, 300400}).
-define(LOGID02, {ff6a6c3ae0734a65b2845c39d223b249, 100200}).
-define(LOGID03, {bd8b9bc7d0ef48a98f6286c779781985, 100400}).
-define(LOGID04, {a55b446c103646b2a84543a907c13378, 100400}).
-define(LOGID05, {b31ad32a76a94d78b866b2e168f9f7d3, 100200}).
-define(LOGID06, {c2ba31819c384d76966d6bdb0cf02250, 100200}).
-define(LOGID07, {d406dffdf263429eac4e64b7c52f7276, 200202}).
-define(LOGID08, {e9f0397b5b814c3abd55f3a474815f6c, 200200}).
-define(LOGID09, {f819220efa73409fa896aaafc0c21d30, 200200}).
-define(LOGID10, {a953af5734984f39b1c26a3541a1f48f, 200200}).
-define(LOGID11, {c2763c7aba514282a48a87dee13e7738, 300401}).
-define(LOGID12, {a887733cce7949f5a1d041441ccdfdfc, 200202}).
-define(LOGID13, {bc9ed80e61cb441c952422c649a3ff3e, 200201}).
-define(LOGID14, {bfde238fb72f420996cb52eaeda474df, 200200}).
-define(LOGID15, {cb4c75e200f74a279ca3373e37eb7f81, 200400}).
-define(LOGID16, {b68c94425c1f40aca74ef33e13eccbfc, 300400}).
-define(LOGID17, {d77a63a5b17c4f0499fc9360083aa7a1, 300202}).
-define(LOGID18, {ef4efe6bb336491990b99cc1113adb85, 200202}).
-define(LOGID19, {e61cd5d46b344504aad275c1f4831f90, 200202}).
-define(LOGID20, {cd035f8d88aa45cca84b67a4227c8cd6, 200401}).
-define(LOGID21, {d330d615c9a84f9dadd4a327fa3ac2fe, 200201}).

%MESSAGE IDs and SATUS CODEs for hello_handler
-define(LOGID22, {c68287edeaac43fe96f49161eb8f44ba, 400200}).
-define(LOGID23, {d3496f96a1154dad96e5ec469dda164b, 400200}).
-define(LOGID24, {e2f93d4effd2479fa7a6291b2c9da1dd, 210500}).
-define(LOGID25, {fc965b4e012648578c379fe0faae3461, 210200}).
-define(LOGID26, {a7237645a763489591c1559b7ec586b8, 210202}).
-define(LOGID27, {aabd551dcc334fe093954d19b4dcc445, 210501}).
-define(LOGID28, {b9ab6b39134542fdbf0fabd7a88e8a19, 210202}).
-define(LOGID29, {c457bf8eb8574927bb1285bc32c18aff, 210200}).
-define(LOGID30, {da812da4d818443a872767687edd47f3, 210202}).
-define(LOGID31, {b3543661a8b2472984874c9c7325fa81, 210202}).
-define(LOGID32, {aeb8bd41d1ed47048e6ea6056cade3ac, 210202}).
-define(LOGID33, {aee0c58903824a139d1b1df028c223d6, 210202}).

%MESSAGE IDs and SATUS CODEs for hello_router
-define(LOGID37, {aee45db4a0a4465687588331edc8bde4, 400500}).
-define(LOGID38, {cfe782643a4e49a581a94c2ae146e683, 400200}).

%MESSAGE IDs and SATUS CODEs for hello_http_client
-define(LOGID39, {a95801c548ad4f508f315334ff79f199, 100410}).
-define(LOGID40, {eab6276590fc4d84996662f4c13a271a, 400500}).
-define(LOGID41, {a48b0a9d2aaa477494fade8ace87f20f, 400202}).
-define(LOGID42, {d3cbc27eb6ab43f2b2dfc9114c122054, 200410}).

%MESSAGE IDs and SATUS CODEs for hello_zmq_client
-define(LOGID43, {d80498ec152342eba94827f005123d27, 400200}).
-define(LOGID44, {e6fa0b7a7ffb45f29a359eca6776b369, 200211}).
-define(LOGID45, {eafe96b96fb447c59ba19d8452ee16f8, 400202}).

%MESSAGE IDs and SATUS CODEs for hello_htp_listener
-define(LOGID46, {cb26c528ecb2474db3078ca1156ba7ba, 210510}).

%MESSAGE IDs and SATUS CODEs for hello_zmq_listener
-define(LOGID47, {c0ea6cda30b64f10be3db0ee5fdc22d1, 110500}).
-define(LOGID48, {dea8dfd8818e432bb61a64ca88e35a9e, 210510}).
-define(LOGID49, {e849ee8a79b2435394e81b51894a617c, 400202}).

%MESSAGE IDs and SATUS CODEs for hello_metrics
-define(LOGID50, {adcfa613af3d40538bddb1e5fd178139, 500500}).

%MESSAGE IDs and SATUS CODEs for hello_registry
-define(LOGID51, {bb0ea6d2b9c74a78afa724889ceee0b5, 600500}).
-define(LOGID52, {da5cd3be4435462dafff47b1bbc34a62, 600501}).
-define(LOGID53, {a3fcdc9a486247d1af5ea679d0f724e9, 610202}).
-define(LOGID54, {bd8ad7e2a04940e8bc4fd7417d2907aa, 600200}).
-define(LOGID55, {ab5f921ed6e7488293241dc27eaf102e, 600500}).
-define(LOGID56, {b5886b31a45d422bb02422aaf072797b, 500500}).
-define(LOGID57, {af5e3e79776642e4851709b58e7ea790, 400200}).

%MESSAGE IDs and SATUS CODEs for hello_service
-define(LOGID58, {eda0efbe7f6d490490e00ae4055629d2, 400500}).

%MESSAGE IDs and SATUS CODEs for hello_supervisor
-define(LOGID59, {a3b473cec1c841d0ada3232fafe8d081, 000400}).
-define(LOGID60, {be1432aa9c074facae66b9d6786de2e5, 000400}).

%MESSAGE IDs and SATUS CODEs for hello_proto_jsonrpc
-define(LOGID61, {ab65930009164e7fb6419b615b4ae4b5, 100420}).
-define(LOGID62, {bd81f08a8fc94502bcbd07975e952016, 310400}).
-define(LOGID63, {c796ed5e01f0455fb09c42e0f04198c1, 310400}).
