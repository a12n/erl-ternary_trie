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
-export([find/2, fold/3, from_list/1, get/2, get/3, is_key/2, keys/1,
         map/2, merge/2, new/0, put/3, remove/2, size/1, to_list/1]).

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
-spec find(nonempty_string(), ternary_trie()) -> {ok, any()} | error.

find(_Key = "", _Trie) ->
    error(badarg);

find(Key, Trie) ->
    case find_node(Key, Trie) of
        #node{ value = Value } when Value =/= undefined ->
            {ok, Value};
        _Other ->
            error
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fold(fun((nonempty_string(), any(), any()) -> any()),
           any(), ternary_trie()) -> any().

fold(Fun, Acc, Trie) ->
    fold(Fun, Acc, Trie, _RevPrefix = "").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_list([{nonempty_string(), any()}]) -> ternary_trie().

from_list(List) ->
    lists:foldl(fun({K, V}, T) -> put(K, V, T) end, new(), List).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(nonempty_string(), ternary_trie()) -> any().

get(Key, Trie) ->
    case find(Key, Trie) of
        {ok, Value} ->
            Value;
        error ->
            error(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(nonempty_string(), ternary_trie(), any()) -> any().

get(Key, Trie, Default) ->
    case find(Key, Trie) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_key(nonempty_string(), ternary_trie()) -> boolean().

is_key(Key, Trie) ->
    case find(Key, Trie) of
        {ok, _Value} ->
            true;
        _Other ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec keys(ternary_trie()) -> [nonempty_string()].

keys(Trie) ->
    fold(fun(K, _V, Keys) -> [K | Keys] end, [], Trie).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec map(fun((nonempty_string(), any()) -> any()), ternary_trie()) ->
                 ternary_trie().

map(Fun, Trie) ->
    map(Fun, Trie, _RevPrefix = "").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec merge(ternary_trie(), ternary_trie()) -> ternary_trie().

merge(Trie1, Trie2) ->
    fold(fun(K, V, T) -> put(K, V, T) end, Trie1, Trie2).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> ternary_trie().

new() ->
    _Empty = undefined.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(nonempty_string(), any(), ternary_trie()) -> ternary_trie().

put(_Key = [C], Value, _Trie = undefined) ->
    #node{ char = C, value = Value };

put(_Key = [C | Other], Value, _Trie = undefined) ->
    #node{ char = C, mid = put(Other, Value, undefined) };

put(Key = [C | _Other], Value, Node = #node{ char = Char, left = Left })
  when C < Char ->
    Node#node{ left = put(Key, Value, Left) };

put(Key = [C | _Other], Value, Node = #node{ char = Char, right = Right })
  when C > Char ->
    Node#node{ right = put(Key, Value, Right) };

put(_Key = [_C], Value, Node) ->
    Node#node{ value = Value };

put(_Key = [_C | Other], Value, Node = #node{ mid = Mid }) ->
    Node#node{ mid = put(Other, Value, Mid) }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove(nonempty_string(), ternary_trie()) -> ternary_trie().

remove(_Key, Trie) ->
    %% TODO
    Trie.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec size(ternary_trie()) -> non_neg_integer().

size(_Trie) ->
    %% TODO
    0.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_list(ternary_trie()) -> [{nonempty_string(), any()}].

to_list(Trie) ->
    fold(fun(K, V, List) -> [{K, V} | List] end, [], Trie).

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
    match(Pattern, Trie, _RevPrefix = "", _List = []).

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
    case find_node(Prefix, Trie) of
        undefined ->
            [];
        Node ->
            fold(fun(K, V, List) ->
                         [{K, V} | List]
                 end, [], Node, lists:reverse(Prefix))
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prefix_keys(string(), ternary_trie()) -> [nonempty_string()].

prefix_keys(Prefix, Trie) ->
    case find_node(Prefix, Trie) of
        undefined ->
            [];
        Node ->
            fold(fun(K, _V, Keys) ->
                         [K | Keys]
                 end, [], Node, lists:reverse(Prefix))
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fold(fun((nonempty_string(), any(), any()) -> any()),
           any(), ternary_trie(), string()) -> any().

fold(_Fun, Acc, _Trie = undefined, _RevPrefix) ->
    Acc;

fold(Fun, Acc, _Node = #node{ char = Char,
                              value = Value,
                              left = Left,
                              mid = Mid,
                              right = Right }, RevPrefix) ->
    RevPrefix1 = [Char | RevPrefix],
    RightAcc = fold(Fun, Acc, Right, RevPrefix),
    ValueAcc = case Value of
                   undefined ->
                       RightAcc;
                   _Other ->
                       Fun(lists:reverse(RevPrefix1), Value, RightAcc)
               end,
    MidAcc = fold(Fun, ValueAcc, Mid, RevPrefix1),
    _LeftAcc = fold(Fun, MidAcc, Left, RevPrefix).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_node(string(), ternary_trie()) -> #node{} | undefined.

find_node(Key = [C | _Other], _Node = #node{ char = Char, left = Left })
  when C < Char ->
    find_node(Key, Left);

find_node(Key = [C | _Other], _Node = #node{ char = Char, right = Right })
  when C > Char ->
    find_node(Key, Right);

find_node(_Key = [_C], Node = #node{}) ->
    Node;

find_node(_Key = [_C | Other], _Node = #node{ mid = Mid }) ->
    find_node(Other, Mid);

find_node(_Key = "", Node) ->
    Node;

find_node(_Key, _Node = undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec map(fun((nonempty_string(), any()) -> any()), ternary_trie(),
          string()) -> ternary_trie().

map(_Fun, Node = undefined, _RevPrefix) ->
    Node;

map(Fun, Node = #node{ char = Char,
                       value = Value,
                       left = Left,
                       mid = Mid,
                       right = Right }, RevPrefix) ->
    RevPrefix1 = [Char | RevPrefix],
    Node#node{ value = case Value of
                           undefined ->
                               Value;
                           _Other ->
                               Fun(lists:reverse(RevPrefix1), Value)
                       end,
               left = map(Fun, Left, RevPrefix),
               mid = map(Fun, Mid, RevPrefix1),
               right = map(Fun, Right, RevPrefix) }.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec match(string(), ternary_trie(), string(),
            [{nonempty_string(), any()}]) ->
                   [{nonempty_string(), any()}].

match(_Pattern, _Node = undefined, _RevPrefix, List) ->
    List;

match(Pattern = [C | Other],
      #node{ char = Char, value = Value,
             left = Left, mid = Mid, right = Right },
      RevPrefix, List) ->
    RevPrefix1 = [Char | RevPrefix],
    List1 =
        if (C > Char) or (C == $.) ->
                match(Pattern, Right, RevPrefix, List);
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
                        match(Other, Mid, RevPrefix1, List1)
                end;
           true ->
                List1
        end,
    _List3 =
        if (C < Char) or (C == $.) ->
                match(Pattern, Left, RevPrefix, List2);
           true ->
                List2
        end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

find_2_test_() ->
    [ ?_assertError(badarg, find("", new())),
      ?_assertEqual(error, find("A", new())),
      ?_assertEqual({ok, 12}, find("A", from_list([{"A", 12}]))) ].

fold_3_test_() ->
    [ ?_assertEqual(0, fold(fun(_K, V, A) -> V + A end, 0, new())),
      ?_assertEqual(6, fold(fun(_K, V, A) -> V + A end, 0,
                            from_list([{"A", 1}, {"BC", 2}, {"ZYX", 3}]))),
      ?_assertEqual(true, fold(fun(_K, V, A) -> (V and A) end, true,
                               from_list([{"Z", true}, {"GHC", true}, {"YUA", true}]))) ].

from_list_1_test_() ->
    List = [{"HKM", 12}, {"LM", 10}, {"OPQ", 19}],
    [ ?_assertEqual(List, to_list(from_list(List))),
      ?_assertEqual([], to_list(from_list([]))),
      ?_assertEqual([{"A", 1}, {"CB", 2}, {"ZHK", 3}],
                    to_list(from_list([{"ZHK", 3}, {"CB", 2}, {"A", 1}]))) ].

get_2_test_() ->
    [ ?_assertError(badarg, get("", from_list([{"A", 1}]))),
      ?_assertError(badarg, get("A", new())),
      ?_assertError(badarg, get("B", from_list([{"A", 1}]))),
      ?_assertEqual(12, get("CBL", from_list([{"CBL",12}]))) ].

get_3_test_() ->
    [ ?_assertError(badarg, get("", from_list([{"A", 1}]), 12)),
      ?_assertEqual(12, get("B", from_list([{"A", 1}]), 12)),
      ?_assertEqual(1, get("A", from_list([{"A", 1}]), 12)) ].

is_key_2_test_() ->
    [ ?_assert(is_key("A", from_list([{"A", 1}, {"AA", 2}]))),
      ?_assertError(badarg, is_key("", new())),
      ?_assertNot(is_key("A", new())) ].

keys_1_test_() ->
    [ ?_assertEqual(["ABC", "GHC", "KFC"], keys(from_list([{"GHC", 12}, {"KFC", 33}, {"ABC", 99}]))),
      ?_assertEqual([], keys(from_list([]))) ].

map_2_test_() ->
    [ ?_assertEqual([], to_list(map(fun erlang:'++'/2, from_list([])))),
      ?_assertEqual([ {"AAA", "AAABBB"},
                      {"AA", "AABB"},
                      {"A", "AB"} ],
                    to_list(map(fun erlang:'++'/2, from_list([ {"A", "B"},
                                                               {"AA", "BB"},
                                                               {"AAA", "BBB"} ])))) ].

merge_2_test_() ->
    [ ?_assertEqual([ {"AA", 2}, {"A", 1}, {"BB", 3}, {"B", 4} ],
                    to_list(merge(from_list([ {"A", 1},
                                              {"B", 444} ]),
                                  from_list([ {"AA", 2},
                                              {"BB", 3},
                                              {"B", 4} ])))) ].

size_1_test_() ->
    [ ?_assertEqual(0, ?MODULE:size(new())),
      ?_assertEqual(1, ?MODULE:size(from_list([{"A", 1}]))),
      ?_assertEqual(1, ?MODULE:size(from_list([{"A", 1}, {"A", 2}]))),
      ?_assertEqual(2, ?MODULE:size(from_list([{"A", 1}, {"AA", 2}]))) ].

-endif.
