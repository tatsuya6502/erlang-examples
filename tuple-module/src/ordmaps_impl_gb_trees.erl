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

-module(ordmaps_impl_gb_trees).

-export([new/0,
         add/3,
         update/3,
         find/2,
         remove/2,
         keys/1,
         to_list/1
        ]).

-behaviour(ordmaps).

-spec new() -> ordmaps:data().
new() ->
    gb_trees:empty().

-spec add(ordmaps:key(), ordmaps:value(), ordmaps:data())
         -> ordmaps:data() | no_return().
add(Key, Val, Tree) ->
    gb_trees:insert(Key, Val, Tree).

-spec update(ordmaps:key(), ordmaps:value(), ordmaps:data())
            -> ordmaps:data() | no_return().
update(Key, Val, Tree) ->
    try gb_trees:update(Key, Val, Tree) of
        Tree1 ->
            Tree1
    catch
        error:_ ->
            error({key_not_found, Key})
    end.

-spec find(ordmaps:key(), ordmaps:data())
          -> {ok, ordmaps:value()} | key_not_found.
find(Key, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Val} ->
            {ok, Val};
        none ->
            key_not_found
    end.

-spec remove(ordmaps:key(), ordmaps:data()) -> ordmaps:data().
remove(Key, Tree) ->
    try gb_trees:delete(Key, Tree) of
        Tree1 ->
            Tree1
    catch
        error:_ ->
            Tree
    end.

-spec keys(ordmaps:data()) -> [ordmaps:key()].
keys(Tree) ->
    gb_trees:keys(Tree).

-spec to_list(ordmaps:data()) -> [{ordmaps:key(), ordmaps:value()}].
to_list(Tree) ->
    gb_trees:to_list(Tree).
