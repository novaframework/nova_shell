-module(nova_shell_terminal_controller).
-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(_State) ->
    logger:info("Init called in websocket"),
    Binding = erl_eval:new_bindings(),
    {ok, #{binding => Binding}}.

websocket_handle({text, <<"!!_comp_!!", Rest/binary>>}, State) ->
    Search = lists:reverse(binary_to_list(Rest)),
    case edlin_expand:expand(Search) of
        {yes, [], List} ->
            %% Traverse list and send result
            Res = lists:map(fun({Module, _Beam}) ->
                                    erlang:list_to_binary(Module)
                            end, List),
            {reply, {binary, term_to_binary(#{<<"action">> => <<"completion_list">>,
                                  <<"items">> => Res})}, State};
        {yes, Completion, _List} ->
            Json = term_to_binary(#{<<"action">> => <<"completion">>,
                          <<"completion">> => erlang:list_to_binary(Completion)}),
            {reply, {binary, Json}, State};
        _ ->
            Json = term_to_binary(#{<<"action">> => <<"completion_not_found">>}),
            {reply, {binary, Json}, State}
    end;
websocket_handle({text, <<194, Data/binary>>}, #{binding := Binding} = State) ->
    logger:info("Received: ~p Bindings: ~p", [Data, Binding]),
    Data0 = erlang:binary_to_term(Data),
    try eval(Data0, Binding) of
        {value, Value, NewBinding} ->
            Json = term_to_binary(#{<<"action">> => <<"command_success">>,
                                    <<"result">> => erlang:list_to_binary(io_lib:format("~p", [Value]))}),
            {reply, {binary, Json}, State#{binding => NewBinding}}
    catch
        _:Why ->
            logger:info("Failed command ~p", [Why]),
            Json = term_to_binary(#{<<"action">> => <<"command_failed">>,
                                    <<"reason">> => erlang:list_to_binary(io_lib:format("~p", [Why]))}),
            {reply, {binary, Json}, State}
    end.

websocket_info(Msg, State) ->
    logger:info("Received info: ~p", [Msg]),
    {ok, State}.


json(Map) ->
    json:encode(Map, [binary, maps]).

eval(Str, Binding) when is_binary(Str) ->
    eval(erlang:binary_to_list(Str), Binding);
eval(Str, Binding) ->
    {ok, Ts, _} = erl_scan:string(Str),
    Ts1 = case lists:reverse(Ts) of
              [{dot,_}|_] -> Ts;
              TsR -> lists:reverse([{dot, 1} | TsR])
          end,
    {ok, Expr} = erl_parse:parse_exprs(Ts1),
    erl_eval:exprs(Expr, Binding).
