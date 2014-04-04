-module(zabbix_sender).
-author("Igor Prots <prots.igor@gmail.com>").

-export([start/0, stop/0]).
-export([send_item/1, send_item/2, send_list_items/1, send_list_items/2,
    send_from_file/1, send_from_file/0]).

-type item() :: {binary()|string(), binary()|string()|integer()}.
-type advanced_item() :: {binary()|string(), binary()|string(), binary()|string()|integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start() -> ok.
start() ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(?MODULE, LoadedApps) of
        true ->
            true;
        false ->
            ok = application:load(?MODULE)
    end,
    {ok, Apps} = application:get_key(?MODULE, applications),
    [ok = application:start(A) || A <- Apps ++ [?MODULE], not lists:member(A, RunningApps)],
    ok.

-spec stop() -> ok.
stop() ->
    application:stop(zabbix_sender).

-spec send_item(item()) -> {ok, binary()} | {error, binary()}.
send_item({Key, Value}) ->
    {ok, Data} = convert_data({Key, Value}),
    send(Data).

-spec send_item(item(), string()) -> {ok, binary()} | {error, binary()}.
send_item({Key, Value}, FromHost) ->
    {ok, Data} = convert_data({Key, Value}, FromHost),
    send(Data).

-spec send_list_items([item()]) -> {ok, binary()} | {error, binary()}.
send_list_items(DataList) ->
    {ok, Data} = convert_data_list(DataList),
    send(Data).

-spec send_list_items([item()], string()) -> {ok, binary()} | {error, binary()}.
send_list_items(DataList, FromHost) ->
    {ok, Data} = convert_data_list(DataList, FromHost),
    send(Data).

-spec send_from_file() -> {ok, binary()} | {error, binary()}.
send_from_file() ->
    {ok, Data} = convert_data_from_file(),
    send(Data).

-spec send_from_file(string()) -> {ok, binary()} | {error, binary()}.
send_from_file(FilePath) ->
    {ok, Data} = convert_data_from_file(FilePath),
    send(Data).

%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send(binary()) -> {ok, binary()}.
send(Data) ->
    {ok, Socket} = init(),
    gen_tcp:send(Socket, Data),
    Answer = get_response(Socket),
    close(Socket),
    Answer.

-spec init() -> {ok, port()}.
init() ->
    {ok, ZabbixHost} = application:get_env(zabbix_sender, zabbix_host),
    {ok, ZabbixPort} = application:get_env(zabbix_sender, zabbix_port),
    {ok, Socket} = gen_tcp:connect(ZabbixHost, ZabbixPort,
        [binary, {packet, 0}, {active, false}]),
    {ok, Socket}.

-spec close(port()) -> ok.
close(Socket) ->
    ok = gen_tcp:close(Socket).

-spec prepare_data(item()| advanced_item()) -> {binary(), binary()}|{binary(), binary(), binary()}.
prepare_data({Key, Value}) when is_binary(Key), is_binary(Value) ->
    {Key, Value};
prepare_data({Key, Value}) when is_list(Key), is_list(Value) ->
    {list_to_binary(Key), list_to_binary(Value)};
prepare_data({Key, Value}) when is_list(Key), is_integer(Value) ->
    {list_to_binary(Key), integer_to_binary(Value)};
prepare_data({Key, Value}) when is_binary(Key), is_integer(Value) ->
    {Key, integer_to_binary(Value)};
prepare_data({FromHost, Key, Value}) when is_binary(FromHost), is_binary(Key), is_binary(Value) ->
    {FromHost, Key, Value};
prepare_data({FromHost, Key, Value}) when is_list(FromHost), is_list(Key), is_list(Value) ->
    {list_to_binary(FromHost), list_to_binary(Key), list_to_binary(Value)};
prepare_data({FromHost, Key, Value}) when is_list(FromHost), is_list(Key), is_integer(Value) ->
    {list_to_binary(FromHost), list_to_binary(Key), integer_to_binary(Value)}.

-spec convert_data(item()) -> {ok, binary()}.
convert_data({Key, Value}) ->
    {ok, FromHost} = application:get_env(zabbix_sender, sender_host),
    convert_data({Key, Value}, FromHost).

-spec convert_data(item(), string()) -> {ok, binary()}.
    convert_data({Key, Value}, FromHost) ->
    {BinKey, BinValue} = prepare_data({Key, Value}),
    Header = <<"ZBXD", 1:8/integer>>,
    JsonMessage = jsx:encode([{<<"request">>, <<"sender data">>},{<<"data">>, [
        [{<<"host">>, list_to_binary(FromHost)},
            {<<"key">>, BinKey},{<<"value">>, BinValue}]
    ]}]),
    Length = byte_size(JsonMessage),
    SizeHeader = <<Length:64/little-integer>>,
    Data = <<Header/binary, SizeHeader/binary, JsonMessage/binary>>,
    {ok, Data}.

-spec convert_data_list([item()]) -> {ok, binary()}.
convert_data_list(DataList) ->
    {ok, FromHost} = application:get_env(zabbix_sender, sender_host),
    convert_data_list(DataList, FromHost).

-spec convert_data_list([item()], string()) -> {ok, binary()}.
convert_data_list(DataList, FromHost) ->
    Header = <<"ZBXD", 1:8/integer>>,
    JsonMessage = jsx:encode([{<<"request">>, <<"sender data">>},
        {<<"data">>, convert_data_list(DataList, FromHost, [])}]),
    Length = byte_size(JsonMessage),
    SizeHeader = <<Length:64/little-integer>>,
    Data = <<Header/binary, SizeHeader/binary, JsonMessage/binary>>,
    {ok, Data}.

-spec convert_data_list(list(), string(), list()) -> list().
convert_data_list([], _FromHost, Acc) ->
    Acc;
convert_data_list([{Key, Value}|T], FromHost, Acc) ->
    {BinKey, BinValue} = prepare_data({Key, Value}),
    convert_data_list(T, FromHost, [[{<<"host">>, list_to_binary(FromHost)},
        {<<"key">>, BinKey},{<<"value">>, BinValue}]|Acc]).

-spec convert_data_from_file() -> {ok, binary()}.
convert_data_from_file() ->
    {ok, FilePath} = application:get_env(zabbix_sender, send_file_path),
    convert_data_from_file(FilePath).

-spec convert_data_from_file(string()) -> {ok, binary()}.
convert_data_from_file(FilePath) ->
    {ok, DataList} = file:consult(FilePath),
    Header = <<"ZBXD", 1:8/integer>>,
    JsonMessage = jsx:encode([{<<"request">>, <<"sender data">>},
        {<<"data">>, convert_consulted_data_list(DataList, [])}]),
    Length = byte_size(JsonMessage),
    SizeHeader = <<Length:64/little-integer>>,
    Data = <<Header/binary, SizeHeader/binary, JsonMessage/binary>>,
    {ok, Data}.

-spec convert_consulted_data_list(list(), list()) -> list().
convert_consulted_data_list([], Acc) ->
    Acc;
convert_consulted_data_list([{FromHost, Key, Value}|T], Acc) ->
    {BinFromHost, BinKey, BinValue} = prepare_data({FromHost, Key, Value}),
    convert_consulted_data_list(T, [[{<<"host">>, BinFromHost},
        {<<"key">>, BinKey},{<<"value">>, BinValue}]|Acc]).

-spec get_response(port()) -> {ok, binary()} | {error, binary()}.
get_response(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, <<_Header:5/binary, _Length:8/binary, Body/binary>>} ->
            parse_answer(Body);
        {ok, <<"ZBXD", 1:8/integer>>} ->
            {ok, <<"ACCEPTED!">>};
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_answer(binary()) -> {ok, binary()}.
parse_answer(MessageBody) ->
    [_Header|Info] = jsx:decode(MessageBody),
    [{_, ProcessedInfo}] = Info,
    {ok, ProcessedInfo}.
