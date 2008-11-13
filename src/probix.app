{application, probix,
 [{description, "probix"},
  {vsn, "0.01"},
  {modules, [
    probix,
    probix_app,
    probix_sup,
    probix_web,
    probix_deps
  ]},
  {registered, []},
  {mod, {probix_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
