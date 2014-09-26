%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@bestmx.ru>
%%% @doc
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(ternary_trie).

%% Types
-export_type([ternary_trie/0, t/0]).

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

-record(node, { char  :: char(),
                value :: any(),
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
-spec get(nonempty_string(), ternary_trie()) -> value().

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
-spec insert(nonempty_string(), any(), ternary_trie()) -> ternary_trie().

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
-spec lookup(nonempty_string(), ternary_trie()) -> {ok, any()} | undefined.

lookup(_Key = "", _Trie) ->
    error(badarg);

lookup(Key, Trie) ->
    case lookup_node(Key, Trie) of
        #node{ value = Value } when Value =/= undefined ->
            {ok, Value};
        _Other ->
            undefined
    end.

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
-spec from_keys([nonempty_string()]) -> ternary_trie().

from_keys(Keys) ->
    from_keys(Keys, new()).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_keys([nonempty_string()], ternary_trie()) -> ternary_trie().

from_keys(Keys, Trie) ->
    lists:foldl(fun(K, T) -> insert(K, true, T) end, Trie, Keys).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_list([{nonempty_string(), any()}]) -> ternary_trie().

from_list(List) ->
    from_list(List, new()).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_list([{nonempty_string(), any()}], ternary_trie()) -> ternary_trie().

from_list(List, Trie) ->
    lists:foldl(fun({K, V}, T) -> insert(K, V, T) end, Trie, List).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec keys(ternary_trie()) -> [nonempty_string()].

keys(Trie) ->
    %% TODO
    lists:map(fun({K, _V}) -> K end, to_list(Trie)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_list(ternary_trie()) -> [{nonempty_string(), any()}].

to_list(Trie) ->
    to_list(Trie, _RevPrefix = "", _List = []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec nearest(nonempty_string(), pos_integer(), ternary_trie()) ->
                     [{nonempty_string(), any()}].

nearest(_Key, _Distance, _Trie) ->
    %% TODO
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec nearest_keys(nonempty_string(), pos_integer(), ternary_trie()) ->
                          [nonempty_string()].

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
-spec match(nonempty_string(), ternary_trie()) -> [{nonempty_string(), any()}].

match(Pattern, Trie) ->
    to_list(Trie, _RevPrefix = "", Pattern, _List = []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec match_keys(nonempty_string(), ternary_trie()) -> [nonempty_string()].

match_keys(Pattern, Trie) ->
    %% TODO
    lists:map(fun({K, _V}) -> K end, match(Pattern, Trie)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prefix(string(), ternary_trie()) -> [{nonempty_string(), any()}].

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
-spec prefix_keys(string(), ternary_trie()) -> [nonempty_string()].

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
-spec lookup_node(string(), ternary_trie()) -> #node{} | undefined.

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
-spec to_list(ternary_trie(), string(), [{nonempty_string(), any()}]) ->
                     [{nonempty_string(), any()}].

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

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_list(ternary_trie(), string(), string(),
              [{nonempty_string(), any()}]) ->
                     [{nonempty_string(), any()}].

to_list(_Node = undefined, _RevPrefix, _Pattern, List) ->
    List;

to_list(#node{ char = Char, value = Value,
               left = Left, mid = Mid, right = Right },
        RevPrefix, Pattern = [C | Other], List) ->
    RevPrefix1 = [Char | RevPrefix],
    List1 =
        if (C > Char) or (C == $.) ->
                to_list(Right, RevPrefix, Pattern, List);
           true ->
                List
        end,
    List2 =
        if (C == Char) or (C == $.) ->
                case Other of
                    [] ->
                        case Value of
                            undefined ->
                                List1;
                            _Some ->
                                Key = lists:reverse(RevPrefix1),
                                [{Key, Value} | List1]
                        end;
                    _NonEmpty ->
                        to_list(Mid, RevPrefix1, Other, List1)
                end;
           true ->
                List1
        end,
    List3 =
        if (C < Char) or (C == $.) ->
                to_list(Left, RevPrefix, Pattern, List2);
           true ->
                List2
        end.
