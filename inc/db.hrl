-record(irc_bot_db, {botname, nick, realname, servers, channels, password}).
-record(relay_client, {from, to}).
-record(factoid_tree, {id, parent}).
-record(factoid_data, {name, id, ro, revision, data}).
-record(plugin_record, {name}).

