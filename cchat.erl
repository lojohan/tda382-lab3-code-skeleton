% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0, send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().
    
send_job(Server,F,Inputs) ->
    ClientTasks = genserver:request(list_to_atom(Server),{assign_tasks, self(), F, Inputs}),
    Results = receive_job(ClientTasks),
    Results.
    
receive_job([]) -> [];

receive_job([HeadClientTask | RemainingClientTasks]) ->
    {Client,_} = HeadClientTask,
    % wait for results to come in
    Results = 
        receive
            {task_result, Client, Result} ->
                [Result | receive_job(RemainingClientTasks)]
        end,
    Results.
