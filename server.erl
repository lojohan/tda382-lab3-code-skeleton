-module(server).
-export([handle/2, initial_state/1, handle_channel/2, send_message/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{servername = ServerName}.

%% ---------------------------------------------------------------------------


%% Spawn a new thread sending a message to clients in channel.
send_message_proc(Pid, Msg) ->
    spawn(server, send_message, [Pid, Msg]).
    
%% ---------------------------------------------------------------------------

%% Relay a message to all clients in Channel, except to client with pid From.
send_message(Pid, Msg) ->
    genserver:request(Pid, Msg).


%% ---------------------------------------------------------------------------
%% A channel Process

handle_channel(St, {client_join, Pid}) ->
    NewState = St#channel_st{ clients = sets:add_element(Pid, St#channel_st.clients) },
    {reply, ok, NewState};
    
handle_channel(St, {client_leave, Pid}) ->
    NewState = St#channel_st{ clients = sets:del_element(Pid, St#channel_st.clients) },
    {reply, ok, NewState};

handle_channel(St, {client_msg, From, Nick, Msg}) ->
    Channel = St#channel_st.name,
    MessageToClients = {incoming_msg, atom_to_list(Channel), Nick, Msg},
    OtherClientsInChannel = sets:to_list(sets:del_element(From, St#channel_st.clients)),
    io:fwrite("Channel ~p: sending message from ~p to clients ~p~n", [Channel, From, OtherClientsInChannel]),
    _Results = lists:map(fun (Client) -> send_message_proc(Client, MessageToClients) end, OtherClientsInChannel),
    {reply, ok, St}.

%% ---------------------------------------------------------------------------


%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

%% Handle a client sending a message in a channel
handle(St, { msg, From, Nick, Channel, Msg}) ->
    %io:fwrite("Server received MESSAGE!: ~p ~p ~p ~n", [From, Nick, Channel]),
    ChannelAtom = list_to_atom(Channel),
    genserver:request(ChannelAtom, {client_msg, From, Nick, Msg}),
    Response = ok,
    {reply, Response, St};


%% Handle Nick change requests. 
handle(St, { request_nick, From, Nick }) ->
    %io:fwrite("Server received nick change request!: ~p ~p~n", [From, Nick]),
    %io:fwrite("Server got new client!: ~p ~p ~n", [From, Nick]),
    %io:fwrite("Currently connected clients: ~p~n", [St#server_st.clients]),
    
    NickTaken = lists:any( fun ({Pid1, Nick1}) -> ((Nick1 == Nick) and (Pid1 /= From)) end, St#server_st.clients),
    %io:fwrite("NickTaken: ~p~n", [NickTaken]),
    case NickTaken of
    true ->
        Response = nick_taken,
        %io:fwrite("Server is sending: ~p~n", [Response]),
        {reply, Response, St};
    false ->
        % add to clients list
        NewClients = [{Pid, Nick1} || {Pid, Nick1} <- St#server_st.clients, Pid/=From] ++ [{From, Nick}],
        %io:fwrite("NewClients: ~p~n", [NewClients]),
        NewState = St#server_st { clients = NewClients },
        Response = "Welcome " ++ Nick ++ "!",
        %io:fwrite("Server is sending: ~p~n", [Response]),
        {reply, Response, NewState}
    end ;
    
%% Handle a client disconnecting from server
handle(St, {disconnect, From}) ->
    %io:fwrite("Disconnect from server, client: ~p ~n", [From]),
    NewState = St#server_st { clients = lists:filter( fun ({Pid, _}) -> Pid /= From end, St#server_st.clients) },
    Response = ok,
    {reply, Response, NewState};
    
%% Handle a client joining a channel
handle(St, { joined_channel, From, _Nick, Channel}) ->
    %io:fwrite("Server ~p: ~p joined ~p ~n", [ServerName, Nick, 
    ChannelAtom = list_to_atom(Channel),
    ChannelExists = lists:member(ChannelAtom, St#server_st.channels),
    NewState =
        if not ChannelExists ->
            % start new channel proc
            NewChannelState = #channel_st{name=ChannelAtom},
            genserver:start(ChannelAtom, NewChannelState, fun server:handle_channel/2),
            St#server_st{ channels = [ChannelAtom | St#server_st.channels] };
        true -> St
        end, 
    %io:fwrite("Channel: ~p now contains ~p ~n", [Channel, NewState#server_st.channels]),
    
    % add client to channel
    genserver:request(ChannelAtom, {client_join, From}),
    
    Response = ok,
    {reply, Response, NewState};

%% Handle a client leaving a channel
handle(St, { left_channel, From, _Nick, Channel}) ->
    ChannelAtom = list_to_atom(Channel),
    genserver:request(ChannelAtom, {client_leave, From}),
    Response = ok,
    {reply, Response, St}.
    

