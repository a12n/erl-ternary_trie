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

-record(node, { char  :: char(),
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

fetch_keys(Trie) ->
    fetch_keys(Trie, "", []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find(key(), ternary_trie()) -> {ok, value()} | false.

find(Key = [C | _Other], Node = #node{ char = Char, left = Left })
  when C < Char ->
    find(Key, Left);

find(Key = [C | _Other], Node = #node{ char = Char, right = Right })
  when C > Char ->
    find(Key, Right);

find(_Key = [_C], Node = #node{ value = Value })
  when Value =/= undefined ->
    {ok, Value};

find(_Key = [_C | Other], Node = #node{ mid = Mid }) ->
    find(Other, Mid);

find(_Key, _TST) ->
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

store(_Key = [C], Value, _Trie = undefined) ->
    #node{ char = C, value = Value };

store(_Key = [C | Other], Value, _Trie = undefined) ->
    #node{ char = C, mid = store(Other, Value, undefined) };

store(Key = [C | _Other], Value, Node = #node{ char = Char, left = Left })
  when C < Char ->
    Node#node{ left = store(Key, Value, Left) };

store(Key = [C | _Other], Value, Node = #node{ char = Char, right = Right })
  when C > Char ->
    Node#node{ right = store(Key, Value, Right) };

store(_Key = [_C], Value, Node) ->
    Node#node{ value = Value };

store(_Key = [_C | Other], Value, Node = #node{ mid = Mid }) ->
    Node#node{ mid = store(Other, Value, Mid) }.

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

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_keys(ternary_trie(), string(), [string()]) -> [string()].

fetch_keys(_Node = undefined, _RevPrefix, Keys) ->
    Keys;

fetch_keys(#node{ char = Char, value = Value,
                  left = Left, mid = Mid, right = Right },
           RevPrefix, Keys) ->
    Keys1 = fetch_keys(Right, RevPrefix, Keys),
    Keys2 = case Value of
                undefined ->
                    Keys1;
                _Other ->
                    [lists:reverse([Char | RevPrefix]) | Keys1]
            end,
    Keys3 = fetch_keys(Mid, [Char | RevPrefix], Keys2),
    _Keys4 = fetch_keys(Left, RevPrefix, Keys3).
