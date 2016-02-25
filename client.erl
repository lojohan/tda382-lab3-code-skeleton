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
    Data = { hello_msg, St#client_st.nick},
    io:fwrite("Client is sending: ~p~n", [Data]),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, Data),
    io:fwrite("Client received: ~p~n", [Response]),
    
    New_St = St#client_st{server = ServerAtom},
    {reply, ok, New_St};
    

%% Disconnect from server
handle(St, disconnect) ->
    New_St = St#client_st{server = "", channels = []},
    {reply, ok, New_St} ;

% Join channel
handle(St, {join, Channel}) ->
    % Notify server that we joined a channel
    Data = { joined_channel, self(), St#client_st.nick, Channel },
    _ = genserver:request(St#client_st.server, Data),
    
    % Now update our state
    NewChannels = St#client_st.channels ++ [Channel],
    New_St = St#client_st{channels = NewChannels},
    {reply, ok, New_St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    % Notify server that we left a channel
    Data = { left_channel, self(), St#client_st.nick, Channel },
    _ = genserver:request(St#client_st.server, Data),
    
    % Now update our state
    NewChannels = St#client_st.channels -- [Channel],
    New_St = St#client_st{channels = NewChannels},
    {reply, ok, New_St} ;

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
            {reply, user_not_joined, St}
    end;
   
%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    NewState = St#client_st{ nick=Nick },
    {reply, ok, NewState} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    io:fwrite("~p: Incoming message!: ~p ~p ~p ~n", [self(), Channel, Name, Msg]),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
 
 
 
 
