%% Records used in EMX

%% Stored in a config (d)ets table
-record(emxcontent, { displayname, writetime, writeuser, content, epoch}).

%% Config for a store
-record(emxstoreconfig, { typename, storagetype, storageoptions, extractcontent, tableid, capacityconstraints, epoch, location }).





