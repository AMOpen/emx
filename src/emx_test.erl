-module(emx_test).

-export([test/0]).

-include_lib("records.hrl").

test() ->
	emx_install:clean(),
	application:start(emx),
	Type = #emxtypeinfo{ 
		typename = "system.config",
		description = "Configuration information",
		owner = alan,
		tableprefix = "sconfig",
		tabletype = ram_copies,
		displayinfo = [ application, region, name ],
		keepversions = true,
		latestid = 0,
		compressionlevel = 9},
	emx_admin:create_type(Type),
	Data = #putcontent {
		displayname = "official/system.config/app/sf/test",
		content = "<test></test>",
		writeuser = alan,
		writetime = calendar:universal_time()
		},
	Data2 = Data#putcontent {
		content = "<test2></test2>"
	},
	Data3 = Data#putcontent {
		displayname = "official/system.config/app/ldn/test" },
	emx_admin:put_data(Data),
	emx_admin:put_data(Data2),
	emx_admin:put_data(Data3),
	
	lists:foreach(fun(Version) ->
		BackRecord = emx_admin:get_data("official/system.config/app/sf/test", Version),
		io:format("Got back ~p for version ~p~n", [ BackRecord, Version])
		end, [ latest, 1]),
		
	%% Now need to try out a query
	
	Query = { xand, [ { index, 'defsystem.config', {region, eq, "sf"} } ]},
	
	emx_admin:perform_query(Query).
	
	
	