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
	{logtags, [debug, info, error]}
	]},
  {mod, {emx_app,[]}},
  {start_phases, []}
 ]}.
