%% Copyright (c) 2011-2017, Stefan Hellkvist <stefan@hellkvist.org>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%
%% @author Stefan Hellkvist <stefan@hellkvist.org> [https://blog.hellkvist.org/]
%% @copyright 2011-2017 Stefan Hellkvist
%% @doc Simple replace/match functions for templates. The module gives you either a
%% function to replace placeholders inside a template string given a dictionary or
%% a function for matching out variable values given a template string and a string.
%% Variables are always on the form $(VARIABLE_NAME)


-module(template).

-export([replace/2, match/2]).

%% api

%% @doc replaces variables inside the template string with the values defined by the dictionary
%% @param Template A template string, possibly containing variables on the form $(VARIABLE_NAME)
%% @param Dictionary A map with keys matching the variable names in the template string and with values being strings
%% @returns A list where all variables have been substituted or an error tuple if an error was found
-spec replace(list(), map()) -> list() | {error, any()}.
replace(Template, Dictionary) ->
    try
        replace(Template, [], Dictionary)
    catch
        error:{badkey, K} ->
            {error, {bad_key, K}};

        error:{badmatch, {error, eof}} ->
            {error, eof}
    end.


%% @doc given a template string and a string to match the template against, this extracts a map of variables which
%% satisfies the match
%% @param Template A template string, possibly containing variables on the form $(VARIABLE_NAME)
%% @param String A string that the template will be matched against
%% @returns A map where all variables in the template are keys with values that satisfies the map or an error tuple if an error was found
-spec match(list(), list()) -> map() | {error, any()}.
match(Template, String) ->
    try
        match(Template, String, #{}, {Template, String})
    catch
        error:{badmatch,{error,eof}} ->
            {error, eof}
    end.




%%% internal functions


replace([], Result, _Dict) ->
    lists:reverse(lists:flatten(Result));

replace("$(" ++ Rest, Result, Dict) ->
    {ok, VarName, Transform, RemainsAfterVariable} = read_var_name(Rest),
    Value = eval(VarName, Transform, maps:get(VarName, Dict)),
    replace(RemainsAfterVariable, [lists:reverse(Value) | Result], Dict);

replace([C | RestTemplate], Result, Dict) ->
    replace(RestTemplate, [C | Result], Dict).




match([], [], D, _) ->
    D;

match("$(" ++ Rest, String, D, Arg) ->
    {ok, VarName, Transform, RemainsAfterVariable} = read_var_name(Rest),
    case maps:get(VarName, D, undefined) of
        undefined -> %% if we don't have an existing binding we try all possible bindings
            bind({VarName, Transform}, [], RemainsAfterVariable, String, D, Arg);

        CurrentValue -> %% if we have an existing binding we substitute the value of the variable and try to match
            match(lists:flatten([eval(VarName, Transform, CurrentValue) | RemainsAfterVariable]), String, D, Arg)
    end;

match([C | TemplateRest], [C | StringRest], D, Arg) ->
    match(TemplateRest, StringRest, D, Arg);

match(_, _, _, Arg) ->
    {error, {no_match, Arg}}.



bind({VarName, T}, Value, [], [], D, _) ->
    D#{VarName => eval(VarName, T, lists:reverse(Value))};

bind({VarName, T}, _, [], String, D, _) when length(String) > 0 ->
    D#{VarName => eval(VarName, T, String)};

bind({VarName, T}, Value, Template, [], D, Arg) when length(Template) > 0 ->
    NewD = D#{VarName => eval(VarName, T, lists:reverse(Value))},
    match(Template, [], NewD, Arg);

bind({VarName, T}, AccValue, String, String, D, _) ->
    D#{VarName => eval(VarName, T, lists:reverse(AccValue))};

bind({VarName, T}, AccValue, Template, [C | StringRest] = String, D, Arg) ->
    NewD = D#{VarName => eval(VarName, T, lists:reverse(AccValue))},
    case match(Template, String, NewD, Arg) of
        {error, _Reason} ->
            bind({VarName, T}, [C | AccValue], Template, StringRest, D, Arg);

        YetAnotherD ->
            YetAnotherD
    end.


eval(_, identity, Val) ->
    Val;
eval(VarName, Body, Val) ->
    FunStr = "fun (" ++ VarName ++ ") -> " ++ Body ++ " end.",
    {ok, Tokens, _} = erl_scan:string(FunStr),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {value, Fun, _} = erl_eval:expr(Form, erl_eval:new_bindings()),
    Fun(Val).



%% reads out the variable name from a string where the string starts after the "$(" part of the variable
%% a variable name could either be on the simple form $(NAME) or include a function body to be evaluated to substitute
%% the value of the variable, such as $(NAME|lists:reverse(NAME))
read_var_name(String) ->
    read_var_name(String, []).

read_var_name([], _Res) ->
    {error, eof};
read_var_name("|" ++ Remain, Res) ->
    read_fun_body(Res, Remain, [], 0);
read_var_name(")" ++ Remain, Res) ->
    {ok, lists:reverse(Res), identity, Remain};
read_var_name([C | Remain], Res) ->
    read_var_name(Remain, [C | Res]).


read_fun_body(_VarName, [], _Res, _PBalance) ->
    {error, eof};
read_fun_body(VarName, "(" ++ Remain, Res, PBalance) ->
    read_fun_body(VarName, Remain, [$( | Res], PBalance + 1);
read_fun_body(VarName, ")" ++ Remain, Res, PBalance) when PBalance > 0 ->
    read_fun_body(VarName, Remain, [$) | Res], PBalance - 1);
read_fun_body(VarName, ")" ++ Remain, Res, 0) ->
    {ok, lists:reverse(VarName), lists:reverse(Res), Remain};
read_fun_body(VarName, [C | Remain], Res, PBalance) ->
    read_fun_body(VarName, Remain, [C | Res], PBalance).



%%% EUNIT tests %%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% replace tests
noreplace_test() ->
    ?assertEqual("Hello", template:replace("Hello", #{})).

onereplace_test() ->
    ?assertEqual("Hello", template:replace("$(A)", #{"A" => "Hello"})).

tworeplace_test() ->
    ?assertEqual("Hello mr X and mrs Y",
                 template:replace("Hello $(A) and $(B)",
                                  #{"A" => "mr X",
                                    "B" => "mrs Y"})).

same_replace_test() ->
    ?assertEqual("1+1=11",
                 template:replace("$(A)+$(A)=$(A)$(A)",
                                  #{"A" => "1"})).


not_found_test() ->
    ?assertEqual({error, {bad_key, "A"}},
                 template:replace("$(A)", #{})).

non_terminated_key_test() ->
    ?assertEqual({error, eof},
                 template:replace("$(A", #{})).


%% match tests

match_no_variables_test() ->
    D = match("abc", "abc"),
    ?assertEqual([], maps:keys(D)).

match_empty_strings_test() ->
    D = match("", ""),
    ?assertEqual([], maps:keys(D)).

match_no_match_no_variables_test() ->
    ?assertEqual({error, {no_match, {"abc", "123"}}}, match("abc", "123")).

match_no_match_no_variables2_test() ->
    ?assertEqual({error, {no_match, {"abc", ""}}}, match("abc", "")).

match_only_one_variable_test() ->
    D = match("$(A)", "abc"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("abc", maps:get("A", D)).

match_one_variable_at_beginning_with_remainder_test() ->
    D = match("$(A)abc", "abc"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("", maps:get("A", D)).

match_one_variable_at_end_test() ->
    D = match("abc$(A)", "abc"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("", maps:get("A", D)).

match_one_variable_in_middle_test() ->
    D = match("abc$(A)def", "abc123def"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("123", maps:get("A", D)).

match_two_variables_test() ->
    D = match("abc$(A)#$(B)def", "abc123#321def"),
    ?assertEqual(["A", "B"], maps:keys(D)),
    ?assertEqual("123", maps:get("A", D)),
    ?assertEqual("321", maps:get("B", D)).

match_two_times_test() ->
    D = match("a$(A)$(A)a", "abba"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("b", maps:get("A", D)).

match_bad_variable_test() ->
    ?assertEqual({error, eof}, match("$(A", def)).

match_no_match_test() ->
    ?assertEqual({error, {no_match, {"$(A)a", "b"}}}, match("$(A)a", "b")).

match_no_match2_test() ->
    ?assertEqual({error, {no_match, {"a$(A)a$(A)", "abac"}}}, match("a$(A)a$(A)", "abac")).

match_full_string_to_one_variable_test() ->
    D = match("$(_BODY)", "some string"),
    ?assertEqual("some string", maps:get("_BODY", D)).

match_conflict_test() ->
    D = match("$(A)$(A)", "ab"),
    ?assertEqual({error, {no_match, {"$(A)$(A)", "ab"}}}, D).

match_three_test() ->
    D = match("$(A)$(A)$(A)", "ababab"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("ab", maps:get("A", D)).

match_with_variable_pattern_test() ->
    D = match("$(A)", "$(A)"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("$(A)", maps:get("A", D)).

match_with_complex_variable_test() ->
    D = match("$(A|lists:reverse(A))", "ab"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("ba", maps:get("A", D)).

match_palindrom_test() ->
    D = match("$(A)$(A|lists:reverse(A))", "olassalo"),
    ?assertEqual(["A"], maps:keys(D)),
    ?assertEqual("olas", maps:get("A", D)).

match_non_palindrom_test() ->
    ?assertEqual({error,{no_match,{"$(A)$(A|lists:reverse(A))","abcb"}}},
                 match("$(A)$(A|lists:reverse(A))", "abcb")).

match_reversed_vars_test() ->
    D = match("$(A)a$(A|lists:reverse(A))", "12a21"),
    ?assertEqual("12", maps:get("A", D)).

match_reversed2_vars_test() ->
    D = match("$(A|lists:reverse(A))a$(A)", "12a21"),
    ?assertEqual("21", maps:get("A", D)).

match_complex_vars_test() ->
    ?assertEqual(#{"A" => "Stefan"},
                 template:match("$(A)$(A|lists:reverse(string:to_upper(A)))", "StefanNAFETS")).


%% read_var_name
read_simple_var_test() ->
    ?assertEqual({ok, "A", identity, ""}, read_var_name("A)")),
    ?assertEqual({ok, "A", identity, "rest"}, read_var_name("A)rest")).

read_complex_var_test() ->
    ?assertEqual({ok, "A", "r(A)", ""}, read_var_name("A|r(A))")),
    ?assertEqual({ok, "A", "r(A)", "rest"}, read_var_name("A|r(A))rest")).

read_bad_complex_var_test() ->
    ?assertEqual({error, eof}, read_var_name("A|r(A")).

%% eval test
eval_test() ->
    ?assertEqual("a", eval("A", identity, "a")),
    ?assertEqual("cba", eval("A", "lists:reverse(A)", "abc")).

-endif.
