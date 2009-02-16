{application,probix,
	[
		{description, "probix"},
		{vsn, "0.01"},
		{modules,
			[
				probix,
				probix_app,
				probix_sup,
				probix_db,
				probix_http
			]
		},
		{registered, []},
		{mod, {probix_app, []}},
		{env, []},
		{applications, [kernel, stdlib, crypto, mnesia]}
	]
}.
