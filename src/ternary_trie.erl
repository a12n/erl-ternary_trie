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
-export([get/2, get/3, find/2, fold/3, is_key/2, map/2, merge/2,
         new/0, put/3]).

%% API
-export([from_list/1, keys/1, to_list/1]).

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
-spec get(nonempty_string(), ternary_trie()) -> any().

get(Key, Trie) ->
    case find(Key, Trie) of
        {ok, Value} ->
            Value;
        _Other ->
            error(badarg)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(nonempty_string(), ternary_trie(), any()) -> any().

get(Key, Trie, Default) ->
    try
        get(Key, Trie)
    catch
        error : badarg ->
            Default
    end.

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

%%%===================================================================
%%% API
%%%===================================================================

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
    case find_node(Prefix, Trie) of
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

find_node(Key = [C | _Other], #node{ char = Char, left = Left })
  when C < Char ->
    find_node(Key, Left);

find_node(Key = [C | _Other], #node{ char = Char, right = Right })
  when C > Char ->
    find_node(Key, Right);

find_node(_Key = [_C | Other], #node{ mid = Mid }) ->
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
