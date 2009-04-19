{application, bot_app,
 [{description, "Irc bot"},
  {vsn, "1.0"},
  {modules, [bot_app, bot_supervisor, dict_proc,
             factoid, factoid_parser, flood_monitor,
             flood_policy, irc_bot, irc_lib, irc_lookup,
             msg_interface, p1_db]},
  {registered, [bot_supervisor]},
  {applications, [kernel, stdlib]},
  {mod, {bot_app, []}}
  ]}.
   
