# template

A simple template/replace for erlang lists/strings


## usage

### replacing variables in the template given a dictionary
``` erlang
2> template:replace("Hello $(NAME)", #{"NAME" => "John Doe"}).
"Hello John Doe"
```

``` erlang
3> template:replace("Hello $(GIVEN_NAME) $(SURNAME)", #{"GIVEN_NAME" => "John", "SURNAME" => "Doe"}).
"Hello John Doe"
```

``` erlang
4> template:replace("Hello $(NAME)", #{}).
{error,{bad_key,"NAME"}}
```

``` erlang
5> template:replace("Hello $(NAME", #{"NAME" => "John"}).
{error,eof}
```

### matching template with string producing a dictionary
``` erlang
2> template:match("$(A)bc", "abc").
#{"A" => "a"}
```

``` erlang
3> template:match("a$(A)$(A)a", "abba").
#{"A" => "b"}
```

``` erlang
4> template:match("$(A)bcd$(B)", "bcde").
#{"A" => [],"B" => "e"}
```

``` erlang
5> template:match("$(A)bc$(A)", "abcd").
{error,no_match}
```

``` erlang
6> template:match("$(A)bc$(A", "abcd").
{error,eof}
```
