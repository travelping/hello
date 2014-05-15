hello_test_jsonrpc_compliance_typespec() -> 
{<<"hello_example">>, <<"">>,
 [#rpc{
      name = <<"subtract">>,description = undefined,
      input = 
          #object{
              name = input,description = undefined,
              fields = 
                  [#field{
                       name = <<"subtrahend">>,description = undefined,
                       type = {<<"int64">>,[]},
                       'when' = undefined,default = null,mandatory = true,
                       opts = []},
                   #field{
                       name = <<"minuend">>,description = undefined,
                       type = {<<"int64">>,[]},
                       'when' = undefined,default = null,mandatory = true,
                       opts = []}],
              'when' = undefined,opts = []},
      output = 
          #object{
              name = output,description = undefined,
              fields = 
                  [#field{
                       name = <<"result">>,description = undefined,
                       type = {<<"int64">>,[]},
                       'when' = undefined,default = null,mandatory = false,
                       opts = []}],
              'when' = undefined,opts = []},
      fields = [],opts = []}]}.
