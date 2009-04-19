-record(irc_client_info, {nick, realname="", servers, password}).
-record(irc_bot, {botname, nick, realname="", servers, password, channels}).


-record(irc_command, {state,          % The initial staet to set cmd_state to
                      func,           % The function to run upon completion
                      command,        % The command to send to the irc server
                      handler         % Function to handle incoming data for this command
                      }).
