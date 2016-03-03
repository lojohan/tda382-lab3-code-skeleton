% This record defines the structure of the client process.
% It contains the following fields:
%   nick: the current Nick of this client
%   gui: the name (or Pid) of the GUI process.
%   server: the atom identifying the server that the client is currently connected to.
%   channels: a list of the names of the channels we are in.
-record(client_st, {nick, gui, server = undefined, channels = []}).

% This record defines the structure of the server process.
% It contains the following fields:
%   servername: the atom identifying the server.
%   clients: a list of tuples like {ClientPid, ClientNick} identifying the clients currently connected.
%   channels: a list of channel atoms of this server.
-record(server_st, {servername, clients = [], channels = [] }).


-record(channel_st, {name, clients=sets:new()}).
