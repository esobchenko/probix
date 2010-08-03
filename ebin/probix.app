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
		{env,
         [
          { probix_host, "0.0.0.0" },
          { probix_port, "8000" }
         ]
        },
		{applications, [kernel, stdlib, crypto, mnesia]}
	]
}.
