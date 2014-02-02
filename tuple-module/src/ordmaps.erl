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

-module(ordmaps).

-export(
   [new/1,
    add/3,
    update/3,
    find/2,
    remove/2,
    keys/1,
    to_list/1
   ]).


%% ===================================
%% Types, Records
%% ===================================

-export_type(
   [map_type/0,
    ordmap/0,
    key/0,
    value/0,
    data/0
   ]).

-record(?MODULE, {
           impl_mod :: module(),
           data :: term()
          }).

-type map_type() :: map | gb_tree.
-type ordmap()   :: #?MODULE{}.
-type key()      :: term().
-type value()    :: term().
-type data()     :: map() | gb_tree().


%% ===================================
%% Behaviour - ordmaps
%% ===================================

-callback new() -> data().
-callback add(key(), value(), data()) -> data() | no_return().
-callback update(key(), value(), data()) -> data() | no_return().
-callback find(key(), data()) -> {ok, value()} | key_not_found.
-callback remove(key(), data()) -> data().
-callback keys(data()) -> [key()].
-callback to_list(data()) -> [{key(), value()}].

%% ===================================
%% API Functions
%% ===================================

-spec new(map_type()) -> ordmap().
new(map) ->
    ImplMod = ordmaps_impl_maps,
    Data = ImplMod:new(),
    #?MODULE{impl_mod=ImplMod, data=Data};
new(gb_tree) ->
    ImplMod = ordmaps_impl_gb_trees,
    Data = ImplMod:new(),
    #?MODULE{impl_mod=ImplMod, data=Data}.

-spec add(key(), value(), ordmap()) -> ordmap() | no_return().
add(Key, Val, #?MODULE{impl_mod=ImplMod, data=Data}=Ordmap) ->
    Data1 = ImplMod:add(Key, Val, Data),
    Ordmap#?MODULE{data=Data1}.

-spec update(key(), value(), ordmap()) -> ordmap() | no_return().
update(Key, Val, #?MODULE{impl_mod=ImplMod, data=Data}=Ordmap) ->
    Data1 = ImplMod:update(Key, Val, Data),
    Ordmap#?MODULE{data=Data1}.

-spec find(key(), ordmap()) -> {ok, value()} | not_found.
find(Key, #?MODULE{impl_mod=ImplMod, data=Data}) ->
    ImplMod:find(Key, Data).

-spec remove(key(), ordmap()) -> ordmap().
remove(Key, #?MODULE{impl_mod=ImplMod, data=Data}=Ordmap) ->
    Data1 = ImplMod:remove(Key, Data),
    Ordmap#?MODULE{data=Data1}.

-spec keys(ordmap()) -> [key()].
keys(#?MODULE{impl_mod=ImplMod, data=Data}) ->
    ImplMod:keys(Data).

-spec to_list(ordmap()) -> [key()].
to_list(#?MODULE{impl_mod=ImplMod, data=Data}) ->
    ImplMod:to_list(Data).
