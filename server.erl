-module(server).
-export([handle/2, initial_state/1, send_message/5]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{servername = ServerName}.

%% ---------------------------------------------------------------------------


%% send a message to clients in channel

send_message_to_channel(St, From, Nick, Channel, Msg) ->
    spawn(server, send_message, [St, From, Nick, Channel, Msg]).

send_message(St, From, Nick, Channel, Msg) ->
    MessageToClients = {incoming_msg, Channel, Nick, Msg},
    ClientsInChannel = dict:fetch(Channel, St#server_st.channels) -- [From],
    if ClientsInChannel /= [] ->
        io:fwrite("Server sending to clients: ~p~n", [ClientsInChannel]),
        Results = lists:map(fun (Client) -> genserver:request(Client, MessageToClients) end, ClientsInChannel),
        io:fwrite("Client responses: ~p~n", [Results]);
    true -> no_clients_to_send_to
    end.

%% handle/2 handles requests from clients



%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, { msg, From, Nick, Channel, Msg}) ->
    io:fwrite("Server received MESSAGE!: ~p ~p ~p ~n", [From, Nick, Channel]),
    send_message_to_channel(St, From, Nick, Channel, Msg),
    Response = ok,
    {reply, Response, St};
    
handle(St, { hello_msg, Nick }) ->
    io:fwrite("Server got new client!: ~p ~n", [Nick]),
    Response = "Welcome " ++ Nick ++ "!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St};
    
handle(St=#server_st{servername=ServerName}, { joined_channel, From, Nick, Channel}) ->
    io:fwrite("Server ~p: ~p joined ~p ~n", [ServerName, Nick, Channel]),
    NewState = St#server_st{ channels=dict:append(Channel, From, St#server_st.channels ) },
    io:fwrite("Channel: ~p now contains ~p ~n", [Channel, NewState#server_st.channels]),
    
    Msg = Nick ++ " joined the channel!",
    send_message_to_channel(NewState, self(), ServerName, Channel, Msg),
    
    Response = ok,
    {reply, Response, NewState};
    
handle(St=#server_st{servername=ServerName}, { left_channel, From, Nick, Channel}) ->
    io:fwrite("Server ~p: ~p left ~p ~n", [ServerName, Nick, Channel]),
    NewState = St#server_st{ channels=dict:filter( fun(Chan, Pid) -> ( (Chan/=Channel) or (Pid/=From) ) end, St#server_st.channels) },
    io:fwrite("Channel: ~p now contains ~p ~n", [Channel, NewState#server_st.channels]),
    
    Msg = Nick ++ " left the channel!",
    send_message_to_channel(NewState, self(), ServerName, Channel, Msg),
    
    Response = ok,
    {reply, Response, NewState}.
