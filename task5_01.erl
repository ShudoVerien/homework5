-module(key_value_benchmark).
-export([benchmark/0]).

benchmark() ->
    % Підготовка даних
    ets_table = ets:new(ets_table, [set, public]),
    dets_file = dets:open_file(dets_table, [{file, "dets_table.dets"}]),
    Map = maps:new(),
    GBTree = gb_trees:empty(),
    Key = key1,
    Value = value1,
    %% ETS
    {ETS_Insert_Time, _} = timer:tc(fun() -> ets:insert(ets_table, {Key, Value}) end),
    {ETS_Read_Time, _} = timer:tc(fun() -> ets:lookup(ets_table, Key) end),
    {ETS_Update_Time, _} = timer:tc(fun() -> ets:insert(ets_table, {Key, value2}) end),
    {ETS_Delete_Time, _} = timer:tc(fun() -> ets:delete(ets_table, Key) end),
    %% DETS
    {DETS_Insert_Time, _} = timer:tc(fun() -> dets:insert(dets_table, {Key, Value}) end),
    {DETS_Read_Time, _} = timer:tc(fun() -> dets:lookup(dets_table, Key) end),
    {DETS_Update_Time, _} = timer:tc(fun() -> dets:insert(dets_table, {Key, value2}) end),
    {DETS_Delete_Time, _} = timer:tc(fun() -> dets:delete(dets_table, Key) end),
    %% MAPS
    {MAP_Insert_Time, Map1} = timer:tc(fun() -> maps:put(Key, Value, Map) end),
    {MAP_Read_Time, _} = timer:tc(fun() -> maps:get(Key, Map1) end),
    {MAP_Update_Time, Map2} = timer:tc(fun() -> maps:put(Key, value2, Map1) end),
    {MAP_Delete_Time, Map3} = timer:tc(fun() -> maps:remove(Key, Map2) end),
    %% GB_TREES
    {GB_Insert_Time, GBTree1} = timer:tc(fun() -> gb_trees:insert(Key, Value, GBTree) end),
    {GB_Read_Time, _} = timer:tc(fun() -> gb_trees:get(Key, GBTree1) end),
    {GB_Update_Time, GBTree2} = timer:tc(fun() -> gb_trees:insert(Key, value2, GBTree1) end),
    {GB_Delete_Time, GBTree3} = timer:tc(fun() -> gb_trees:delete(Key, GBTree2) end),
    %% Закриття DETS
    dets:close(dets_file),
    %% Вивід результатів
    io:format("~nComparison of Key-Value Mechanisms:\n"),
    io:format("~p~30s~10d μs\n", ["ETS Insert", "", ETS_Insert_Time]),
    io:format("~p~30s~10d μs\n", ["ETS Read", "", ETS_Read_Time]),
    io:format("~p~30s~10d μs\n", ["ETS Update", "", ETS_Update_Time]),
    io:format("~p~30s~10d μs\n", ["ETS Delete", "", ETS_Delete_Time]),
    io:format("~p~30s~10d μs\n", ["DETS Insert", "", DETS_Insert_Time]),
    io:format("~p~30s~10d μs\n", ["DETS Read", "", DETS_Read_Time]),
    io:format("~p~30s~10d μs\n", ["DETS Update", "", DETS_Update_Time]),
    io:format("~p~30s~10d μs\n", ["DETS Delete", "", DETS_Delete_Time]),
    io:format("~p~30s~10d μs\n", ["MAP Insert", "", MAP_Insert_Time]),
    io:format("~p~30s~10d μs\n", ["MAP Read", "", MAP_Read_Time]),
    io:format("~p~30s~10d μs\n", ["MAP Update", "", MAP_Update_Time]),
    io:format("~p~30s~10d μs\n", ["MAP Delete", "", MAP_Delete_Time]),
    io:format("~p~30s~10d μs\n", ["GB Insert", "", GB_Insert_Time]),
    io:format("~p~30s~10d μs\n", ["GB Read", "", GB_Read_Time]),
    io:format("~p~30s~10d μs\n", ["GB Update", "", GB_Update_Time]),
    io:format("~p~30s~10d μs\n", ["GB Delete", "", GB_Delete_Time]).
