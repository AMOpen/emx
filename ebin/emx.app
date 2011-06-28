%% This is the application resource file (.app file) for the 'base'
%% application.
{application, emx,
 [{description, "EMX XML Cache"},
  {vsn, "1.0"},
  {modules, []},
  {registered,[]},
  {applications, [kernel,stdlib]},
  {env, [
   	{logdir, '~s'},
	{logsize, 5000000 },
	{logtags, [debug, info, error]},
	{datadir, 'data' },
	{webhost, localhost },
	{webport, 8665 },
	{weblogdir, 'data'},
	{emxconfig, {ets, [{keypos, 2}]}},
	{defaultstore, { 
		emxstoreconfig, 
		"default", 
		ets, 
		[{keypos, 2}], 
		undefined, 
		[ { records, 20000 }, {age, 1200}, {archive, 180}, { size, 50000000}],
		0,
		system }},
	{nodes, [emx1@localhost] },
	{setupstore, true }
	]},
  {mod, {emx_app,[]}},
  {start_phases, [{jobs, []}]}
 ]}.