-module(my_cache).
-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).
-define(TABLE_PREFIX, cache_).

%% Створити таблицю
create(TableName) ->
    ets:new(?TABLE_PREFIX ++ TableName, [named_table, public, set]).
%% Додавання без обмеження часу
insert(TableName, Key, Value) ->
    Timestamp = calendar:universal_time(),
    ets:insert(?TABLE_PREFIX ++ TableName, {Key, Value, Timestamp, infinity}),
    ok.
%% Додавання з обмеженням часу
insert(TableName, Key, Value, TTL) ->
    ExpiryTime = calendar:universal_time(),
    Timestamp = calendar:universal_time(),
    UpdatedExpiry = calendar:datetime_to_gregorian_seconds(ExpiryTime) + TTL,
    ets:insert(?TABLE_PREFIX ++ TableName, {Key, Value, Timestamp, UpdatedExpiry}),
    ok.
%% Пошук запису
lookup(TableName, Key) ->
    case ets:lookup(?TABLE_PREFIX ++ TableName, Key) of
        [{Key, Value, _, infinity}] ->
            Value;
        [{Key, Value, _, Expiry}] ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            if
                Now < Expiry -> Value;
                true -> undefined
            end;
        [] -> undefined
    end.
%% Видалення застарілих записів
delete_obsolete(TableName) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    ObsoleteKeys = [Key || {Key, _, _, Expiry} <- ets:tab2list(?TABLE_PREFIX ++ TableName), Expiry =/= infinity, Now >= Expiry],
    [ets:delete(?TABLE_PREFIX ++ TableName, Key) || Key <- ObsoleteKeys],
    ok.
