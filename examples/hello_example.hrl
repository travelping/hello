typespec() -> 
{<<"hello_example">>,
 [#typedef{
   name = <<"Status">>,description = undefined,
   type = {enumeration,[<<"ok">>,<<"error">>]},
   default = undefined,opts = []},
  #object{
   name = <<"rpc_error">>,description = undefined,
   fields = 
    [#field{
      name = <<"code">>,description = undefined,
      type = {<<"int16">>,[]},
      'when' = undefined,default = null,mandatory = false,opts = []},
     #field{
      name = <<"message">>,description = undefined,
      type = {string,undefined,[]},
      'when' = undefined,default = null,mandatory = false,opts = []}],
   'when' = undefined,opts = []},
  #object{
   name = <<"result">>,description = undefined,
   fields = 
    [#field{
      name = <<"Status">>,description = <<"Result of this operation">>,
      type = {enumeration,[<<"ok">>,<<"error">>]},
      'when' = undefined,default = null,mandatory = false,opts = []},
     #choice{
      name = <<"result">>,description = undefined,
      cases = 
       [#'case'{
         name = <<"error">>,description = undefined,
         fields = 
          [#field{
            name = <<"code">>,description = undefined,
            type = {<<"int16">>,[]},
            'when' = undefined,default = null,mandatory = false,opts = []},
           #field{
            name = <<"message">>,description = undefined,
            type = {string,undefined,[]},
            'when' = undefined,default = null,mandatory = false,opts = []}],
         'when' = 
          {comp,'=',
           {path,rel,
            {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
           {literal,<<"error">>}},
         opts = []},
        #'case'{
         name = <<"ok">>,description = undefined,
         fields = 
          [#choice{
            name = <<"Data">>,description = undefined,
            cases = 
             [#'case'{
               name = <<"Boolean">>,description = undefined,
               fields = 
                [#field{
                  name = <<"Ok">>,description = undefined,
                  type = {<<"boolean">>,[]},
                  'when' = undefined,default = null,mandatory = false,
                  opts = []}],
               'when' = undefined,opts = []},
              #'case'{
               name = <<"Items">>,description = undefined,
               fields = 
                [#object{
                  name = <<"Result">>,description = undefined,fields = [],
                  'when' = undefined,opts = []}],
               'when' = undefined,opts = []}],
            'when' = undefined,default = undefined,mandatory = false,
            opts = []}],
         'when' = 
          {comp,'=',
           {path,rel,
            {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
           {literal,<<"ok">>}},
         opts = []}],
      'when' = undefined,default = undefined,mandatory = false,opts = []}],
   'when' = undefined,opts = []},
  #rpc{
   name = <<"echo">>,description = undefined,
   input = 
    #object{
     name = input,description = undefined,
     fields = 
      [#field{
        name = <<"text">>,description = undefined,
        type = {string,undefined,[]},
        'when' = undefined,default = <<>>,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   output = 
    #object{
     name = output,description = undefined,
     fields = 
      [#field{
        name = <<"Status">>,description = <<"Result of this operation">>,
        type = {enumeration,[<<"ok">>,<<"error">>]},
        'when' = undefined,default = null,mandatory = false,opts = []},
       #choice{
        name = <<"result">>,description = undefined,
        cases = 
         [#'case'{
           name = <<"error">>,description = undefined,
           fields = 
            [#field{
              name = <<"code">>,description = undefined,
              type = {<<"int16">>,[]},
              'when' = undefined,default = null,mandatory = false,opts = []},
             #field{
              name = <<"message">>,description = undefined,
              type = {string,undefined,[]},
              'when' = undefined,default = null,mandatory = false,opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"error">>}},
           opts = []},
          #'case'{
           name = <<"ok">>,description = undefined,
           fields = 
            [#choice{
              name = <<"Data">>,description = undefined,
              cases = 
               [#'case'{
                 name = <<"Boolean">>,description = undefined,
                 fields = 
                  [#field{
                    name = <<"Ok">>,description = undefined,
                    type = {<<"boolean">>,[]},
                    'when' = undefined,default = null,mandatory = false,
                    opts = []}],
                 'when' = undefined,opts = []},
                #'case'{
                 name = <<"Items">>,description = undefined,
                 fields = 
                  [#object{
                    name = <<"Result">>,description = undefined,fields = [],
                    'when' = undefined,opts = []}],
                 'when' = undefined,opts = []}],
              'when' = undefined,default = undefined,mandatory = false,
              opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"ok">>}},
           opts = []}],
        'when' = undefined,default = undefined,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   fields = [],opts = []},
  #rpc{
   name = <<"append">>,description = undefined,
   input = 
    #object{
     name = input,description = undefined,
     fields = 
      [#field{
        name = <<"str1">>,description = undefined,
        type = {string,undefined,[]},
        'when' = undefined,default = <<>>,mandatory = false,opts = []},
       #field{
        name = <<"str2">>,description = undefined,
        type = {string,undefined,[]},
        'when' = undefined,default = <<>>,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   output = 
    #object{
     name = output,description = undefined,
     fields = 
      [#field{
        name = <<"Status">>,description = <<"Result of this operation">>,
        type = {enumeration,[<<"ok">>,<<"error">>]},
        'when' = undefined,default = null,mandatory = false,opts = []},
       #choice{
        name = <<"result">>,description = undefined,
        cases = 
         [#'case'{
           name = <<"error">>,description = undefined,
           fields = 
            [#field{
              name = <<"code">>,description = undefined,
              type = {<<"int16">>,[]},
              'when' = undefined,default = null,mandatory = false,opts = []},
             #field{
              name = <<"message">>,description = undefined,
              type = {string,undefined,[]},
              'when' = undefined,default = null,mandatory = false,opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"error">>}},
           opts = []},
          #'case'{
           name = <<"ok">>,description = undefined,
           fields = 
            [#choice{
              name = <<"Data">>,description = undefined,
              cases = 
               [#'case'{
                 name = <<"Boolean">>,description = undefined,
                 fields = 
                  [#field{
                    name = <<"Ok">>,description = undefined,
                    type = {<<"boolean">>,[]},
                    'when' = undefined,default = null,mandatory = false,
                    opts = []}],
                 'when' = undefined,opts = []},
                #'case'{
                 name = <<"Items">>,description = undefined,
                 fields = 
                  [#object{
                    name = <<"Result">>,description = undefined,fields = [],
                    'when' = undefined,opts = []}],
                 'when' = undefined,opts = []}],
              'when' = undefined,default = undefined,mandatory = false,
              opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"ok">>}},
           opts = []}],
        'when' = undefined,default = undefined,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   fields = [],opts = []},
  #rpc{
   name = <<"enum_test">>,description = undefined,
   input = 
    #object{
     name = input,description = undefined,
     fields = 
      [#field{
        name = <<"atom">>,description = undefined,
        type = {enumeration,[<<"a">>,<<"b">>,<<"c">>]},
        'when' = undefined,default = null,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   output = 
    #object{
     name = output,description = undefined,
     fields = 
      [#field{
        name = <<"Status">>,description = <<"Result of this operation">>,
        type = {enumeration,[<<"ok">>,<<"error">>]},
        'when' = undefined,default = null,mandatory = false,opts = []},
       #choice{
        name = <<"result">>,description = undefined,
        cases = 
         [#'case'{
           name = <<"error">>,description = undefined,
           fields = 
            [#field{
              name = <<"code">>,description = undefined,
              type = {<<"int16">>,[]},
              'when' = undefined,default = null,mandatory = false,opts = []},
             #field{
              name = <<"message">>,description = undefined,
              type = {string,undefined,[]},
              'when' = undefined,default = null,mandatory = false,opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"error">>}},
           opts = []},
          #'case'{
           name = <<"ok">>,description = undefined,
           fields = 
            [#choice{
              name = <<"Data">>,description = undefined,
              cases = 
               [#'case'{
                 name = <<"Boolean">>,description = undefined,
                 fields = 
                  [#field{
                    name = <<"Ok">>,description = undefined,
                    type = {<<"boolean">>,[]},
                    'when' = undefined,default = null,mandatory = false,
                    opts = []}],
                 'when' = undefined,opts = []},
                #'case'{
                 name = <<"Items">>,description = undefined,
                 fields = 
                  [#object{
                    name = <<"Result">>,description = undefined,fields = [],
                    'when' = undefined,opts = []}],
                 'when' = undefined,opts = []}],
              'when' = undefined,default = undefined,mandatory = false,
              opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"ok">>}},
           opts = []}],
        'when' = undefined,default = undefined,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   fields = [],opts = []},
  #rpc{
   name = <<"return_error">>,description = undefined,
   input = 
    #object{
     name = input,description = undefined,
     fields = 
      [#field{
        name = <<"code">>,description = undefined,
        type = {<<"int16">>,[]},
        'when' = undefined,default = null,mandatory = false,opts = []},
       #field{
        name = <<"message">>,description = undefined,
        type = {string,undefined,[]},
        'when' = undefined,default = null,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   output = 
    #object{
     name = output,description = undefined,
     fields = 
      [#field{
        name = <<"Status">>,description = <<"Result of this operation">>,
        type = {enumeration,[<<"ok">>,<<"error">>]},
        'when' = undefined,default = null,mandatory = false,opts = []},
       #choice{
        name = <<"result">>,description = undefined,
        cases = 
         [#'case'{
           name = <<"error">>,description = undefined,
           fields = 
            [#field{
              name = <<"code">>,description = undefined,
              type = {<<"int16">>,[]},
              'when' = undefined,default = null,mandatory = false,opts = []},
             #field{
              name = <<"message">>,description = undefined,
              type = {string,undefined,[]},
              'when' = undefined,default = null,mandatory = false,opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"error">>}},
           opts = []},
          #'case'{
           name = <<"ok">>,description = undefined,
           fields = 
            [#choice{
              name = <<"Data">>,description = undefined,
              cases = 
               [#'case'{
                 name = <<"Boolean">>,description = undefined,
                 fields = 
                  [#field{
                    name = <<"Ok">>,description = undefined,
                    type = {<<"boolean">>,[]},
                    'when' = undefined,default = null,mandatory = false,
                    opts = []}],
                 'when' = undefined,opts = []},
                #'case'{
                 name = <<"Items">>,description = undefined,
                 fields = 
                  [#object{
                    name = <<"Result">>,description = undefined,fields = [],
                    'when' = undefined,opts = []}],
                 'when' = undefined,opts = []}],
              'when' = undefined,default = undefined,mandatory = false,
              opts = []}],
           'when' = 
            {comp,'=',
             {path,rel,
              {step,{child,{name,{<<"Status">>,<<>>,<<"Status">>}},[]}}},
             {literal,<<"ok">>}},
           opts = []}],
        'when' = undefined,default = undefined,mandatory = false,opts = []}],
     'when' = undefined,opts = []},
   fields = [],opts = []}]}.
