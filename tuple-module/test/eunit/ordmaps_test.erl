%%% The MIT License
%%%
%%% Copyright (C) 2014 by Tatsuya Kawano
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(ordmaps_test).

-include_lib("eunit/include/eunit.hrl").

-spec ordmap_map_test() -> ok | no_return().
ordmap_map_test() ->
    test_ordmap(ordmaps:new(map)).

-spec ordmap_gb_tree_test() -> ok | no_return().
ordmap_gb_tree_test() ->
    test_ordmap(ordmaps:new(gb_tree)).

-spec test_ordmap(ordmaps:ordmap()) -> ok | no_return().
test_ordmap(M) ->
    ?debugVal(M),
    M1 = M:add({b, name}, "b"),
    M2 = M1:add({b, count}, 1),
    ?assertError({key_exists, {b, count}}, M2:add({b, count}, 3)),
    M3 = M2:add({a, name}, "a"),
    M4 = M3:update({b, count}, 2),
    ?assertError({key_not_found, {a, count}}, M4:update({a, count}, 2)),
    M5 = M4:add({a, count}, 2),
    M6 = M5:remove({a, count}),
    M6 = M6:remove({c, count}),

    ?debugVal(M6),
    ?assertEqual({ok, "a"}, M6:find({a, name})),
    ?assertEqual({ok, 2}, M6:find({b, count})),
    ?assertEqual(key_not_found, M6:find({a, count})),
    ?assertEqual([{a, name}, {b, count}, {b, name}], M6:keys()),
    ?assertEqual([{{a, name}, "a"}, {{b, count}, 2}, {{b, name}, "b"}],
                 M6:to_list()),
    ok.
