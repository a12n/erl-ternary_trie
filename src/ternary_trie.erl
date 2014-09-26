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
-export([get/2, insert/3, lookup/2, new/0]).

%% API
-export([from_keys/1, from_keys/2, from_list/1, from_list/2, keys/1,
         to_list/1]).

%% API
-export([nearest/3, nearest_keys/3]).

%% API
-export([match/2, match_keys/2]).

%% API
-export([prefix/2, prefix_keys/2]).

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
-spec get(key(), ternary_trie()) -> value().

get(Key, Trie) ->
    case lookup(Key, Trie) of
        {ok, Value} ->
            Value;
        _Other ->
            error(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec insert(key(), value(), ternary_trie()) -> ternary_trie().

insert(_Key = [C], Value, _Trie = undefined) ->
    #node{ char = C, value = Value };

insert(_Key = [C | Other], Value, _Trie = undefined) ->
    #node{ char = C, mid = insert(Other, Value, undefined) };

insert(Key = [C | _Other], Value, Node = #node{ char = Char, left = Left })
  when C < Char ->
    Node#node{ left = insert(Key, Value, Left) };

insert(Key = [C | _Other], Value, Node = #node{ char = Char, right = Right })
  when C > Char ->
    Node#node{ right = insert(Key, Value, Right) };

insert(_Key = [_C], Value, Node) ->
    Node#node{ value = Value };

insert(_Key = [_C | Other], Value, Node = #node{ mid = Mid }) ->
    Node#node{ mid = insert(Other, Value, Mid) }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lookup(key(), ternary_trie()) -> {ok, value()} | undefined.

lookup(Key = [C | _Other], #node{ char = Char, left = Left })
  when C < Char ->
    lookup(Key, Left);

lookup(Key = [C | _Other], #node{ char = Char, right = Right })
  when C > Char ->
    lookup(Key, Right);

lookup(_Key = [_C], #node{ value = Value })
  when Value =/= undefined ->
    {ok, Value};

lookup(_Key = [_C | Other], #node{ mid = Mid }) ->
    lookup(Other, Mid);

lookup(_Key = "", _TST) ->
    error(badarg);

lookup(_Key, _TST) ->
    undefined.

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
-spec from_keys([key()]) -> ternary_trie().

from_keys(Keys) ->
    from_keys(Keys, new()).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_keys([key()], ternary_trie()) -> ternary_trie().

from_keys(Keys, Trie) ->
    lists:foldl(fun(K, T) -> insert(K, true, T) end, Trie, Keys).

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
    lists:foldl(fun({K, V}, T) -> insert(K, V, T) end, Trie, List).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec keys(ternary_trie()) -> [key()].

keys(Trie) ->
    %% TODO
    lists:map(fun({K, _V}) -> K end, to_list(Trie)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_list(ternary_trie()) -> [{key(), value()}].

to_list(Trie) ->
    to_list(Trie, _RevPrefix = "", _List = []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec nearest(key(), pos_integer(), ternary_trie()) -> [{key(), value()}].

nearest(_Key, _Distance, _Trie) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec nearest_keys(key(), pos_integer(), ternary_trie()) -> [key()].

nearest_keys(Key, Distance, Trie) ->
    %% TODO
    lists:map(fun({K, _V}) -> K end, nearest(Key, Distance, Trie)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec match(key(), ternary_trie()) -> [{key(), value()}].

match(_Key, _Trie) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec match_keys(key(), ternary_trie()) -> [key()].

match_keys(Key, Trie) ->
    %% TODO
    lists:map(fun({K, _V}) -> K end, match(Key, Trie)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prefix(key(), ternary_trie()) -> [{key(), value()}].

prefix(Prefix, Trie) ->
    case lookup_node(Prefix, Trie) of
        undefined ->
            [];
        Node ->
            to_list(Node, _RevPrefix = lists:reverse(Prefix), _List = [])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prefix_keys(key(), ternary_trie()) -> [key()].

prefix_keys(Prefix, Trie) ->
    %% TODO
    lists:map(fun({K, _V}) -> K end, prefix(Prefix, Trie)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lookup_node(key(), ternary_trie()) -> #node{} | undefined.

lookup_node(Key = [C | _Other], #node{ char = Char, left = Left })
  when C < Char ->
    lookup_node(Key, Left);

lookup_node(Key = [C | _Other], #node{ char = Char, right = Right })
  when C > Char ->
    lookup_node(Key, Right);

lookup_node(_Key = [_C | Other], #node{ mid = Mid }) ->
    lookup_node(Other, Mid);

lookup_node(_Key = "", Node) ->
    Node;

lookup_node(_Key, _Node = undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_list(ternary_trie(), string(), [string()]) -> [string()].

to_list(_Node = undefined, _RevPrefix, List) ->
    List;

to_list(#node{ char = Char, value = Value,
               left = Left, mid = Mid, right = Right },
        RevPrefix, List) ->
    RevPrefix1 = [Char | RevPrefix],
    List1 = to_list(Right, RevPrefix, List),
    List2 = case Value of
                undefined ->
                    List1;
                _Other ->
                    Key = lists:reverse(RevPrefix1),
                    [{Key, Value} | List1]
            end,
    List3 = to_list(Mid, RevPrefix1, List2),
    to_list(Left, RevPrefix, List3).
