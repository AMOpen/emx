%% Records used in EMX

%% A pair of these tables for each type
-record(tcontent, { key, id, version, writetime, writeuser, content}).
-record(theader, { id, latestversion, displayname }).

%% type information

-record(emxtypeinfo, { typename, description, owner, tableprefix, tabletype, displaylength, keepversions }).

%% index information

-record(emxindexinfo, { indexname, tablename, description, fielddefinition, typemappings }).

%% fielddefinition in index information is an array of these

-record(fielddef, { fieldname, generator }).

%% typemappings in index information is an array of these

-record(typemap, { typename, xpathprefix }).

%% An index is a table storing this type
%% contentkey is the key of the original data in the tcontent table, typename is that in the emxtypeinfo
%% indexdata is an array of {atomkey, Value} pairs, where the atoms come from the typemappings in te exmindexinfo table

-record(icontent, { key, typename, contentkey, indexdata }).


