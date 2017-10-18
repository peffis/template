# template

A simple template/replace for erlang lists/strings


## usage

``` erlang
2> template:replace("Hello $(NAME)", #{"NAME" => "John Doe"}).
"Hello John Doe"
```

``` erlang
3> template:replace("Hello $(GIVEN_NAME) $(SURNAME)", #{"GIVEN_NAME" => "John", "SURNAME" => "Doe"}).
"Hello John Doe"
```

## errors
``` erlang
4> template:replace("Hello $(NAME)", #{}).
{error,{bad_key,"NAME"}}
```

``` erlang
5> template:replace("Hello $(NAME", #{"NAME" => "John"}).
{error,eof}
```