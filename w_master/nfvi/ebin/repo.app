%% This is the application resource file (.app file) for the 'base'
%% application.
{application, repo,
[{description, "repo  " },
{vsn, "1.0.0" },
{modules, 
	  [repo_app,repo_sup,repo,repo_lib]},
{registered,[repo]},
{applications, [kernel,stdlib]},
{mod, {repo_app,[]}},
{start_phases, []}
]}.
