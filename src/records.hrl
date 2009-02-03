%% Records used in EMX

%% Record involved in a put or a get
-record(putcontent, { displayname, version, content, writeuser, writetime }).

%% A pair of these tables for each type
-record(emxcontent, { key, id, version, writetime, writeuser, content}).
-record(emxheader, { id, typename, latestversion, displayname }).

%% type information

-record(emxtypeinfo, { typename, description, owner, tableprefix, tabletype, displayinfo, keepversions, latestid, compressionlevel }).

%% Here's a sample of the above
%% { "system.config", "Configuration information", "alan", "sconfig", disc_only_copies, [ application, region, name ], true}

%% index information

-record(emxindexinfo, { indexname, tablename, description, fielddefinition, typemappings }).

%% Here's a sample of the above
%% { "system.config", emxisystemconfig, "Default index for system.config", [ { application, {displayname, 0}}, 
%%										{region, {displayname, 1}},
%%										{name, {displayname, 2}}], [ { "system.config", "/"}]}
										

%% fielddefinition in index information is an array of these

-record(fielddef, { fieldname, generator }).

%% typemappings in index information is an array of these

-record(typemap, { typename, xpathprefix }).

%% An index is a table storing this type
%% contentkey is the key of the original data in the tcontent table, typename is that in the emxtypeinfo
%% indexdata is an array of {atomkey, Value} pairs, where the atoms come from the typemappings in te exmindexinfo table

-record(icontent, { key, typename, id, version, indexdata }).


