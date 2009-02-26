%% Records used in EMX

%% Stored in a config (d)ets table
-record(emxcontent, { displayname, writetime, writeuser, content, content_type, encoding, epoch}).

%% Config for a store
%% location is an array of nodes on which this table is hosted - by default the current node
%% tableid is the tableid of the table ON THIS NODE or the tuple atom { remote }
-record(emxstoreconfig, { typename, storagetype, storageoptions, tableid, capacityconstraints, epoch }).

-define(LOG(Level,Msg,Params), util_flogger:logMsg(self(), ?MODULE, Level, Msg, Params)).



