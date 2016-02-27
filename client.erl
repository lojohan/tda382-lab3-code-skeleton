-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {nick = Nick, gui = GUIName, server = "", channels = [] }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.


%% connect to server already connected to
handle(St, {connect, Server}) when Server == St#client_st.server ->
    {reply, user_already_connected, St};
        
%% Connect to server
handle(St, {connect, Server}) ->
    Data = { hello_msg, self(), St#client_st.nick},
    io:fwrite("Client is sending: ~p~n", [Data]),
    ServerAtom = list_to_atom(Server),
    try genserver:request(ServerAtom, Data, 2000) of
        Response ->
            io:fwrite("Client received: ~p~n", [Response]),
            case Response of nick_taken ->
                {reply, {error, user_already_connected, "name is busy. come again !!!!!!!!!!!!!!!!!!!!!½!"}, St};
            _ ->
                New_St = St#client_st{server = ServerAtom},
                {reply, ok, New_St}
            end
    catch _:_ -> % Atom server_not_reached is returned when the server process cannot be reached for any reason. (As specified on the course webpage)
        {reply, {error, server_not_reached, "server_not_reached !!!!!!!!!!!!!!!!!!!!!½!"}, St}
    end;
        

%% Disconnect from server when not connected
handle(St, disconnect) when St#client_st.server == "" ->
    {reply, {error, user_not_connected, "no connect!!"}, St};

%% Disconnect from server when not left channels
handle(St, disconnect) when St#client_st.channels /= [] ->
    {reply, {error, leave_channels_first, "leave channel first maybe?!!"}, St};

%% Disconnect from server
handle(St, disconnect) ->
    % set the new state
    Server = St#client_st.server,
    NewState = St#client_st{server = ""},
    
    % tell the server
    Data = {disconnect, self()},
    try genserver:request(Server, Data, 2000) of
        Response ->
            io:fwrite("Client received: ~p~n", [Response]),
            {reply, ok, NewState}
    catch _:_ -> % Atom server_not_reached is returned when the server process cannot be reached for any reason. (As specified on the course webpage)
        {reply, {error, server_not_reached, "server_not_reached !!!!!!!!!!!!!!!!!!!!!½!"}, NewState}
    end;

% Join channel
handle(St, {join, Channel}) ->

    AlreadyInChannel = lists:member(Channel, St#client_st.channels),
    io:fwrite("Already in channel: ~p ~p ~p ~n", [AlreadyInChannel, Channel, St#client_st.channels]),
    if AlreadyInChannel ->
        {reply, {error, user_already_joined, "U already in channel!!!"}, St};
    true ->
        % Notify server that we joined a channel
        Data = { joined_channel, self(), St#client_st.nick, Channel },
        _ = genserver:request(St#client_st.server, Data),
        
        % Now update our state
        NewChannels = St#client_st.channels ++ [Channel],
        New_St = St#client_st{channels = NewChannels},
        {reply, ok, New_St}
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    ClientInChannel = lists:member(Channel, St#client_st.channels),
    if ClientInChannel ->
        % Notify server that we left a channel
        Data = { left_channel, self(), St#client_st.nick, Channel },
        _ = genserver:request(St#client_st.server, Data),
        
        % Now update our state
        NewChannels = St#client_st.channels -- [Channel],
        New_St = St#client_st{channels = NewChannels},
        {reply, ok, New_St};
    
    true ->
        {reply, {error, user_not_joined, "Y u not join?"}, St}
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    InChannel = lists:member(Channel, St#client_st.channels),
    case InChannel of
        true ->
            Server = St#client_st.server,
            Data = { msg, self(), St#client_st.nick, Channel, Msg },
            genserver:request(Server, Data),
            {reply, ok, St} ;
        false ->
            {reply, {error, user_not_joined, "Y u not join?"}, St}
    end;
   
%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick offline
handle(St, {nick, Nick}) when St#client_st.server=="" ->
    NewState = St#client_st{ nick=Nick },
    {reply, ok, NewState};

%% Change nick online
handle(St, {nick, Nick}) ->
    Server = St#client_st.server,
    Data = {request_nick, self(), Nick},
    Response = genserver:request(Server, Data),
    %try genserver:request(Server, Data, 10000) of
    %    Response ->
            io:fwrite("Client received: ~p~n", [Response]),
            case Response of nick_taken ->
                {reply, {error, nick_taken, "name is busy. come again !!!!!!!!!!!!!!!!!!!!!½!"}, St};
            _ ->
                NewState = St#client_st{ nick=Nick },
                {reply, ok, NewState}
            end;
    %catch _:_ -> % Atom server_not_reached is returned when the server process cannot be reached for any reason. (As specified on the course webpage)
    %    {reply, {error, server_not_reached, "server_not_reached !!!!!!!!!!!!!!!!!!!!!½!"}, St}
    %end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    io:fwrite("~p: Incoming message!: ~p ~p ~p ~n", [self(), Channel, Name, Msg]),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
 
 
 
 
