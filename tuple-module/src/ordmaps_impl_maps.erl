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

-module(ordmaps_impl_maps).

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
    #{}.

-spec add(ordmaps:key(), ordmaps:value(), ordmaps:data())
         -> ordmaps:data() | no_return().
add(Key, Val, Map) ->
    case maps:is_key(Key, Map) of
        true ->
            error({key_exists, Key});
        false ->
            maps:put(Key, Val, Map)
    end.

-spec update(ordmaps:key(), ordmaps:value(), ordmaps:data())
            -> ordmaps:data() | no_return().
update(Key, Val, Map) ->
    try maps:update(Key, Val, Map) of
        Map1 ->
            Map1
    catch
        error:badarg ->
            error({key_not_found, Key})
    end.

-spec find(ordmaps:key(), ordmaps:data())
          -> {ok, ordmaps:value()} | key_not_found.
find(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, _Val}=Res ->
            Res;
        error ->
            key_not_found
    end.

-spec remove(ordmaps:key(), ordmaps:data()) -> ordmaps:data().
remove(Key, Map) ->
    maps:remove(Key, Map).

-spec keys(ordmaps:data()) -> [ordmaps:key()].
keys(Map) ->
    maps:keys(Map).

-spec to_list(ordmaps:data()) -> [{ordmaps:key(), ordmaps:value()}].
to_list(Map) ->
    maps:to_list(Map).
