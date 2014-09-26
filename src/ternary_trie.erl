%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@bestmx.ru>
%%% @doc
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(ternary_trie).

%% Types
-export_type([key/0, ternary_trie/0, t/0, value/0]).

%% API
-export([from_list/1, from_list/2, new/0]).

%% API
-export([fetch/2, fetch_keys/1, find/2, is_key/2, longest_prefix/2,
         prefix_match/2, size/1, store/3, wildcard_match/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type key() :: binary() | string().

-type value() :: any().

-record(node, { key   :: byte() | char(),
                value :: value(),
                left  :: #node{},
                mid   :: #node{},
                right :: #node{} }).

-opaque ternary_trie() :: (_Empty :: undefined) | (_Root :: #node{}).

-type t() :: ternary_trie().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_list([{key(), value()}]) -> ternary_trie().

from_list(List) ->
    from_list(List, new()).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_list([{key(), value()}], ternary_trie()) -> ternary_trie().

from_list(List, Trie) ->
    lists:foldl(fun({K, V}, T) -> store(K, V, T) end, Trie, List).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> ternary_trie().

new() ->
    _Empty = undefined.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(key(), ternary_trie()) -> value().

fetch(Key, TST) ->
    case find(Key, TST) of
        {ok, Value} ->
            Value;
        _Other ->
            error(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_keys(ternary_trie()) -> [key()].

fetch_keys(_TST) ->
    error(undef).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find(key(), ternary_trie()) -> {ok, value()} | false.

find(QKey = [Char | _Rest], Node = #node{ key = Key, left = Left })
  when Char < Key ->
    find(QKey, Left);

find(QKey = [Char | _Rest], Node = #node{ key = Key, right = Right })
  when Char > Key ->
    find(QKey, Right);

find(_QKey = [_Char], Node = #node{ value = Value })
  when Value =/= undefined ->
    {ok, Value};

find(_QKey = [_Char | Rest], Node = #node{ mid = Mid }) ->
    find(Rest, Mid);

find(_QKey, _TST) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_key(key(), ternary_trie()) -> boolean().

is_key(_Key, _TST) ->
    error(undef).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec longest_prefix(key(), ternary_trie()) -> {ok, key()} | false.

longest_prefix(_Key, _TST) ->
    error(undef).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prefix_match(key(), ternary_trie()) -> [key()].

prefix_match(_Key, _TST) ->
    error(undef).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec size(ternary_trie()) -> non_neg_integer().

size(_TST) ->
    error(undef).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store(key(), value(), ternary_trie()) -> ternary_trie().

store(_NewKey = [Char], Value, _TST = undefined) ->
    #node{ key = Char, value = Value };

store(_NewKey = <<Byte>>, Value, _TST = undefined) ->
    #node{ key = Byte, value = Value };


store(_NewKey = [Char | Rest], Value, _TST = undefined) ->
    #node{ key = Char, mid = store(Rest, Value, undefined) };

store(_NewKey = <<Byte, Rest/bytes>>, Value, _TST = undefined) ->
    #node{ key = Byte, mid = store(Rest, Value, undefined) };


store(NewKey = [Char | _Rest], Value, Node = #node{ key = Key, left = Left })
  when Char < Key ->
    Node#node{ left = store(NewKey, Value, Left) };

store(NewKey = <<Byte, _Rest/bytes>>, Value, Node = #node{ key = Key, left = Left })
  when Byte < Key ->
    Node#node{ left = store(NewKey, Value, Left) };


store(NewKey = [Char | _Rest], Value, Node = #node{ key = Key, right = Right })
  when Char > Key ->
    Node#node{ right = store(NewKey, Value, Right) };

store(NewKey = <<Byte, _Rest/bytes>>, Value, Node = #node{ key = Key, right = Right })
  when Byte > Key ->
    Node#node{ right = store(NewKey, Value, Right) };


store(_NewKey = [_Char], Value, Node) ->
    Node#node{ value = Value };

store(_NewKey = <<_Byte>>, Value, Node) ->
    Node#node{ value = Value };


store(_NewKey = [_Char | Rest], Value, Node = #node{ mid = Mid }) ->
    Node#node{ mid = store(Rest, Value, Mid) };

store(_NewKey = <<_Byte, Rest/bytes>>, Value, Node = #node{ mid = Mid }) ->
    Node#node{ mid = store(Rest, Value, Mid) }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wildcard_match(key(), ternary_trie()) -> [key()].

wildcard_match(_Key, _TST) ->
    error(undef).

%%%===================================================================
%%% Internal functions
%%%===================================================================
