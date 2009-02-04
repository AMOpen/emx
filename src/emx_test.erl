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
	Type2 = Type#emxtypeinfo {
		typename = "trade.aladdin",
		description = "Aladdin trade information",
		displayinfo = [ fund, side, id, invnum ]
		},
	emx_admin:create_type(Type2),
	Index = #emxindexinfo {
			indexname = 'test.config',
			description = "A test xpath based configuration",
			fielddefinition =  [ { one, { xpath, "/test/one/text()" }}, {two, { xpath, "/test/two/text()"}}],
			typemappings = [ { "system.config", "/" }]
		},
	emx_admin:create_index(Index),
			
	Data = #putcontent {
		displayname = "official/system.config/app/sf/test",
		content = "<test><one>1</one><two>2</two></test>",
		writeuser = alan,
		writetime = calendar:universal_time()
		},
	Data2 = Data#putcontent {
		content = "<test><one>un</one><two>deux</two></test>"
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
	
	emx_admin:perform_query(Query),
	
	Query2 = { xand, [ { index, 'test.config', { one, eq, "un" } } ] },
	
	emx_admin:perform_query(Query2).
	
	
	