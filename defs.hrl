% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   channel: the name of the channel we are in.
-record(client_st, {nick, gui, server = "", channels = []}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
-record(server_st, {servername, clients = [], channels = dict:new() }).

%% in clients we put tuples like {ClientPid, ClientNick} 
