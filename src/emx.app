%% This is the application resource file (.app file) for the 'base'
%% application.
{application, emx,
 [{description, "EMX XML Repository"},
  {vsn, "1.0"},
  {modules, []},
  {registered,[]},
  {applications, [kernel,stdlib]},
  {env, [
   	{logdir, '~s'},
	{logsize, 5000000 },
	{logtags, [debug, info, error]},
	{host, localhost },
	{port, 8665 },
	{workingdir, "c:\\clouddata"},
	{emxconfig, {ets, [{keypos, 2}]}},
	{defaultstore, { emxstoreconfig, "default", ets, [{keypos, 2}], false, undefined, [ { records, 5000 }, {age, 3600}, { size, 50000000}],0,local }},
	{nodes, [local] },
	{setupstore, true }
	]},
  {mod, {emx_app,[]}},
  {start_phases, []}
 ]}.
