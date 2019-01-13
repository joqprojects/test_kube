%% This is the application resource file (.app file) for the 'base'
%% application.
{application, divider,
[{description, "divider  " },
{vsn, "1.0.0" },
{modules, 
	  [divider_app,divider_sup,divider,divider_lib]},
{registered,[divider]},
{applications, [kernel,stdlib]},
{mod, {divider_app,[]}},
{start_phases, []}
]}.
