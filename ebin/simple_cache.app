{application, simple_cache,
 [{description, "A simple caching application"},
  {vsn, "0.0.1"},
  {modules, [
			 sc_app,
			 sc_sup
			]},
  {applications, [
				  kernel,
				  stdlib
				  ]},
  {registered, [sc_sup]},
  {mod, {sc_app, []}}
]}.
