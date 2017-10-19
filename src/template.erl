%% Copyright (c) 2011-2017, Stefan Hellqvist <stefan@hellkvist.org>
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

-module(template).

-export([replace/2, match/2]).

%% api

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

-spec match(list(), list()) -> map() | {error, any()}.
match(Template, String) ->
    try
        match(Template, String, #{})
    catch
        error:{badmatch,{error,eof}} ->
            {error, eof}
    end.

%%% internal functions


replace([], Result, _Dict) ->
    lists:reverse(lists:flatten(Result));

replace([$$, $( | Rest], Result, Dict) ->
    {ok, Key, Remain} = read_key(Rest),
    Value = maps:get(Key, Dict),
    replace(Remain, [lists:reverse(Value) | Result], Dict);

replace([C | Rest], Result, Dict) ->
    replace(Rest, [C | Result], Dict).




match([], [], D) ->
    D;

match([C | TemplateRest], [C | StringRest], D) ->
    match(TemplateRest, StringRest, D);

match([$$, $( | Rest], String, D) ->
    {ok, Key, Remain} = read_key(Rest),
    case maps:get(Key, D, undefined) of
        undefined ->
            bind(Key, [], Remain, String, D);

        CurrentValue ->
            match(lists:flatten([CurrentValue | Remain]), String, D)
    end;

match(_, _, _) ->
    {error, no_match}.




bind(Key, Value, [], [], D) ->
    D#{Key => lists:reverse(Value)};

bind(Key, Value, Remain, [], D) when length(Remain) > 0 ->
    NewD = D#{Key => lists:reverse(Value)},
    match(Remain, [], NewD);

bind(Key, AccValue, Remain, Remain, D) ->
    D#{Key => lists:reverse(AccValue)};

bind(Key, AccValue, Remain, [C | StringRest] = String, D) ->
    NewD = D#{Key => lists:reverse(AccValue)},
    case match(Remain, String, NewD) of
        {error, _Reason} ->
            bind(Key, [C | AccValue], Remain, StringRest, D);

        YetAnotherD ->
            YetAnotherD
    end.


read_key(String) ->
    read_key(String, []).

read_key([], _Res) ->
    {error, eof};
read_key([$) | Remain], Res) ->
    {ok, lists:reverse(Res), Remain};
read_key([C | Remain], Res) ->
    read_key(Remain, [C | Res]).


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
    ?assertEqual({error, no_match}, match("abc", "123")).

match_no_match_no_variables2_test() ->
    ?assertEqual({error, no_match}, match("abc", "")).

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
    ?assertEqual({error, no_match}, match("$(A)a", "b")).

match_no_match2_test() ->
    ?assertEqual({error, no_match}, match("a$(A)a$(A)", "abac")).

-endif.
