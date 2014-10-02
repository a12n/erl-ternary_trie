%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@bestmx.ru>
%%% @doc
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(ternary_trie).

%% Types
-export_type([fold_fun/0, map_fun/0, ternary_trie/0, t/0]).

%% API
-export([find/2, fold/3, from_list/1, get/2, get/3, is_key/2, keys/1,
         map/2, merge/2, new/0, put/3, remove/2, size/1, to_list/1]).

%% API
-export([match/2, nearest/3, prefix/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(node, { char :: char(),
                value :: any(),
                left :: #node{},
                mid :: #node{},
                right :: #node{} }).

-record(trie, { size = 0 :: non_neg_integer(),
                root :: #node{} }).

-opaque ternary_trie() :: #trie{}.

-type t() :: ternary_trie().

-type fold_fun() :: fun((_Key :: nonempty_string(),
                         _Value :: any(),
                         _Acc :: any()) -> _NewAcc :: any()).

-type map_fun() :: fun((_Key :: nonempty_string(),
                        _Value :: any()) -> _NewValue :: any()).

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

find(Key, _Trie = #trie{ root = Root }) ->
    case find_node(Key, Root) of
        #node{ value = Value } when Value =/= undefined ->
            {ok, Value};
        _Other ->
            error
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fold(fold_fun(), any(), ternary_trie()) -> any().

fold(Fun, Acc, _Trie = #trie{ root = Root }) ->
    fold_node(Fun, Acc, Root, _RevPrefix = "").

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
-spec map(map_fun(), ternary_trie()) -> ternary_trie().

map(Fun, Trie = #trie{ root = Root }) ->
    Trie#trie{ root = map_node(Fun, Root, _RevPrefix = "") }.

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
    #trie{}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(nonempty_string(), any(), ternary_trie()) -> ternary_trie().

put(Key, Value, Trie = #trie{ size = Size, root = Root }) ->
    {N, NewRoot} = put_node(Key, Value, Root),
    Trie#trie{ size = Size + N, root = NewRoot }.

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

size(#trie{ size = Size }) ->
    Size.

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
-spec match(nonempty_string(), ternary_trie()) -> [{nonempty_string(), any()}].

match(Pattern, _Trie = #trie{ root = Root }) ->
    match_node(Pattern, Root, _RevPrefix = "", _List = []).

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
-spec prefix(string(), ternary_trie()) -> [{nonempty_string(), any()}].

prefix(_Prefix = "", Trie) ->
    to_list(Trie);

prefix(Prefix, _Trie = #trie{ root = Root }) ->
    case find_node(Prefix, Root) of
        #node{ value = Value, mid = Mid } ->
            List0 = case Value of
                        undefined ->
                            [];
                        _Other ->
                            [{Prefix, Value}]
                    end,
            lists:reverse(
              fold_node(fun(K, V, List) -> [{K, V} | List] end,
                        List0, Mid, lists:reverse(Prefix)));
        undefined ->
            []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fold_node(fold_fun(), any(), #node{}, string()) -> any().

fold_node(_Fun, Acc, _Node = undefined, _RevPrefix) ->
    Acc;

fold_node(Fun, Acc, _Node = #node{ char = Char,
                                   value = Value,
                                   left = Left,
                                   mid = Mid,
                                   right = Right }, RevPrefix) ->
    RevPrefix1 = [Char | RevPrefix],
    LeftAcc = fold_node(Fun, Acc, Left, RevPrefix),
    ValueAcc = case Value of
                   undefined ->
                       LeftAcc;
                   _Other ->
                       Fun(lists:reverse(RevPrefix1), Value, LeftAcc)
               end,
    MidAcc = fold_node(Fun, ValueAcc, Mid, RevPrefix1),
    _RightAcc = fold_node(Fun, MidAcc, Right, RevPrefix).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_node(string(), #node{}) -> #node{} | undefined.

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

find_node(_Key, _Node = undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec map_node(map_fun(), #node{}, string()) -> #node{}.

map_node(_Fun, Node = undefined, _RevPrefix) ->
    Node;

map_node(Fun, Node = #node{ char = Char,
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
               left = map_node(Fun, Left, RevPrefix),
               mid = map_node(Fun, Mid, RevPrefix1),
               right = map_node(Fun, Right, RevPrefix) }.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec match_node(string(), #node{}, string(), [{nonempty_string(), any()}]) ->
                        [{nonempty_string(), any()}].

match_node(_Pattern, _Node = undefined, _RevPrefix, List) ->
    List;

match_node(Pattern = [C | Other],
           #node{ char = Char, value = Value,
                  left = Left, mid = Mid, right = Right },
           RevPrefix, List) ->
    RevPrefix1 = [Char | RevPrefix],
    List1 =
        if (C > Char) or (C == $.) ->
                match_node(Pattern, Right, RevPrefix, List);
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
                        match_node(Other, Mid, RevPrefix1, List1)
                end;
           true ->
                List1
        end,
    _List3 =
        if (C < Char) or (C == $.) ->
                match_node(Pattern, Left, RevPrefix, List2);
           true ->
                List2
        end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put_node(nonempty_string(), any(), #node{}) -> {0 | 1, #node{}}.

put_node(_Key = [C], Value, _Node = undefined) ->
    {1, #node{ char = C, value = Value }};

put_node(_Key = [C | Other], Value, _Node = undefined) ->
    {N, NewMid} = put_node(Other, Value, undefined),
    {N, #node{ char = C, mid = NewMid }};

put_node(Key = [C | _Other], Value, Node = #node{ char = Char, left = Left })
  when C < Char ->
    {N, NewLeft} = put_node(Key, Value, Left),
    {N, Node#node{ left = NewLeft }};

put_node(Key = [C | _Other], Value, Node = #node{ char = Char, right = Right })
  when C > Char ->
    {N, NewRight} = put_node(Key, Value, Right),
    {N, Node#node{ right = NewRight }};

put_node(_Key = [_C], Value, Node = #node{ value = undefined }) ->
    {1, Node#node{ value = Value }};

put_node(_Key = [_C], Value, Node) ->
    {0, Node#node{ value = Value }};

put_node(_Key = [_C | Other], Value, Node = #node{ mid = Mid }) ->
    {N, NewMid} = put_node(Other, Value, Mid),
    {N, Node#node{ mid = NewMid }}.

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
