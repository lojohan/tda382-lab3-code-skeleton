-module(server).
-export([handle/2, initial_state/1, send_message/5]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{servername = ServerName}.

%% ---------------------------------------------------------------------------


%% Send a message to clients in channel

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


%% add_client/3
%%   Returns the tuple {Pid, Nick} that were added to/were alreday present in client list. 
add_client(St, Pid, Nick) ->
    NickInList = [{Pid2, Nick2} || {Pid2, Nick2} <- St#server_st.clients, Nick2 == Nick],
    if NickInList/=[] ->
        hd(NickInList); % must be a single tuple
    true ->
        NewClientList = [case X of {Pid, _} -> {Pid, Nick}; X -> X end || X <- St#server_st.clients],
        St#server_st{clients = NewClientList}
    end.


%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, { msg, From, Nick, Channel, Msg}) ->
    io:fwrite("Server received MESSAGE!: ~p ~p ~p ~n", [From, Nick, Channel]),
    send_message_to_channel(St, From, Nick, Channel, Msg),
    Response = ok,
    {reply, Response, St};


%% Handle Nick change requests.    
handle(St, { request_nick, From, Nick }) ->
    io:fwrite("Server received nick change request!: ~p ~p~n", [From, Nick]),
    
    NickTaken = lists:any( fun ({Pid1, Nick1}) -> ((Nick1 == Nick) and (Pid1 /= From)) end, St#server_st.clients),
    case NickTaken of
    true ->
        Response = nick_taken,
        io:fwrite("Server is sending: ~p~n", [Response]),
        {reply, Response, St};
    false ->
        % add to clients list
        NewState = St#server_st { clients = [{Pid, Nick1} || {Pid, Nick1} <- St#server_st.clients, Pid==From] ++ [{From, Nick}] },
        Response = "Welcome " ++ Nick ++ "!",
        io:fwrite("Server is sending: ~p~n", [Response]),
        {reply, Response, NewState}
    end ;
    
handle(St, { hello_msg, From, Nick }) ->
    io:fwrite("Server got new client!: ~p ~p ~n", [From, Nick]),
    io:fwrite("Currently connected clients: ~p~n", [St#server_st.clients]),
    AnyList = lists:filter( fun ({Pid1, Nick1}) -> ((Nick1 == Nick) and (Pid1 /= From)) end, St#server_st.clients),
    io:fwrite("AnyList: ~p~n", [AnyList]),
    NickTaken = lists:any( fun ({Pid1, Nick1}) -> ((Nick1 == Nick) and (Pid1 /= From)) end, St#server_st.clients),
    case NickTaken of
    true ->
        Response = nick_taken,
        io:fwrite("Server is sending: ~p~n", [Response]),
        {reply, Response, St};
    false ->
        % add to clients list
        NewState = St#server_st { clients = [{Pid, Nick1} || {Pid, Nick1} <- St#server_st.clients, Pid==From] ++ [{From, Nick}] },
        Response = "Welcome " ++ Nick ++ "!",
        io:fwrite("Server is sending: ~p~n", [Response]),
        {reply, Response, NewState}
    end ;
    
handle(St, {disconnect, From}) ->
    io:fwrite("Disconnect from server, client: ~p ~n", [From]),
    NewState = St#server_st { clients = lists:filter( fun ({Pid, _}) -> Pid /= From end, St#server_st.clients) },
    Response = ok,
    {reply, Response, NewState};
    
handle(St=#server_st{servername=ServerName}, { joined_channel, From, Nick, Channel}) ->
    io:fwrite("Server ~p: ~p joined ~p ~n", [ServerName, Nick, Channel]),
    NewState = St#server_st{ channels=dict:append(Channel, From, St#server_st.channels ) },
    io:fwrite("Channel: ~p now contains ~p ~n", [Channel, NewState#server_st.channels]),
    
    % a REAL chat server would announce user joins...
    %Msg = Nick ++ " joined the channel!",
    %send_message_to_channel(NewState, self(), ServerName, Channel, Msg),
    
    Response = ok,
    {reply, Response, NewState};
    
handle(St=#server_st{servername=ServerName}, { left_channel, From, Nick, Channel}) ->
    io:fwrite("Server ~p: ~p left ~p ~n", [ServerName, Nick, Channel]),
    OldChannelPids = dict:fetch(Channel, St#server_st.channels),
    io:fwrite("Server ~p: OldChannelPids: ~p ~n", [ServerName, OldChannelPids]),
    NewChannelPids = OldChannelPids -- [From],
    NewChannels = dict:store(Channel, NewChannelPids, St#server_st.channels),
    NewState = St#server_st{ channels=NewChannels },
    io:fwrite("Channel: ~p now contains ~p ~n", [Channel, NewState#server_st.channels]),
    
    % a REAL chat server would announce user leaves...
    %Msg = Nick ++ " left the channel!",
    %send_message_to_channel(NewState, self(), ServerName, Channel, Msg),
    
    Response = ok,
    {reply, Response, NewState}.
