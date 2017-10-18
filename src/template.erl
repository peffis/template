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

-export([replace/2]).

-spec replace(list(), map())
	-> list() | {error, any()}.
replace(String, Dictionary) ->
    try
        replace(String, [], Dictionary)
    catch
        error:{badkey, K} ->
            {error, {bad_key, K}};
        error:{badmatch, {error, eof}} ->
            {error, eof}
    end.



replace([], Result, _Dict) ->
    lists:reverse(lists:flatten(Result));

replace([$$, $( | Rest], Result, Dict) ->
    {ok, Key, Remain} = read_key(Rest),
    Value = maps:get(Key, Dict),
    replace(Remain, [lists:reverse(Value) | Result], Dict);

replace([C | Rest], Result, Dict) ->
    replace(Rest, [C | Result], Dict).




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
-endif.
