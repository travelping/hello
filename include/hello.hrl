-record(rpc_method, {name, params_as = list, description = ""}).
-record(rpc_param,  {name, type = any, optional = false, default, description = ""}).

-define(json_obj_to_record(RecName, JSON),
    hello_service:object_to_record(RecName, record_info(fields, RecName), record_info(size, RecName), #RecName{}, JSON)).
-define(json_obj_into_record(RecName, Defaults, JSON),
    hello_service:object_to_record(RecName, record_info(fields, RecName), record_info(size, RecName), Defaults, JSON)).
-define(record_to_json_obj(RecName, Tup),
    hello_service:record_to_object(RecName, record_info(fields, RecName), record_info(size, RecName), Tup)).
