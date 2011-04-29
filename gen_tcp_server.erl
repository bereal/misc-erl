-module(gen_tcp_server).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_info/2, start_server/2, test/0, test_handle/1, terminate/2]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {socket, loop, port, lpid}).

start_link(Port, Loop) ->
    State = #server_state{port = Port, lpid=undefined, loop=Loop},
    {ok, Pid} = gen_server:start_link(?MODULE, State, []),
    {ok, Pid}.

stop(Server) ->
    gen_server:cast(Server, close).

init(State = #server_state{port = _Port} ) ->
    process_flag(trap_exit, true),
    {ok, State}.

accept_loop(Server, LSocket, {M,F}) ->
    accept_loop(Server, LSocket, fun(Socket) -> M:F(Socket) end);
accept_loop(Server, LSocket, F) ->
    io:format("Waiting, ~w~n", [self()]),
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    F(Socket).
      
accept(State = #server_state{socket=LSocket, loop=Loop}) ->
    io:format("Hello ~w~n", [self()]),
    Me = self(),
    Pid = spawn_link(fun() ->  accept_loop(Me, LSocket, Loop) end),
    io:format("Spawned ~w ~w ~n", [Pid, Loop]),
    State#server_state{lpid = Pid}.
  
handle_cast(listen, State) when State#server_state.lpid==undefined ->
    case gen_tcp:listen(State#server_state.port, ?TCP_OPTIONS) of
	{ok, LSocket} -> {noreply, accept(State#server_state{socket = LSocket})};
	{error, Reason} -> {stop, Reason, State}
    end;
handle_cast({accepted, LPid}, State=#server_state{lpid=_LPid}) ->
    io:format("Accepted~n"),
    {noreply, accept(State)};
handle_cast(close, State) ->
    gen_tcp:close(State#server_state.socket),
    {stop, ok, State}.

handle_info(Info, State) ->
    io:format("Info ~w~n", [Info]),
    {noreply, State}.

test_handle(Socket) ->
1    1/0,
    io:format("Test~n"),
    gen_tcp:close(Socket).

test() ->
    State = #server_state{},
    {ok, S} = start_server(8181, test_handle),
    gen_server:cast(S, listen).

terminate(_A, _B) ->
    ok.
