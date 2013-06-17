{application, simple_cache,
 [{description, "A simple caching application"},
  {vsn, "0.0.1"},
  {modules, []},
  {applications, [
		  kernel,
		  stdlib
		 ]},
  {registered, [sc_sup]},
  {mod, {sc_app, []}}
 ]}.
