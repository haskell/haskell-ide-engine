base
====
.. hie:plugin:: base

   .. hie:command:: version

      return HIE version

      Example::

        {
            "params": {},
            "cmd": "base:version"
        }

   .. hie:command:: plugins

      list available plugins

      Example::

        {
            "params": {},
            "cmd": "base:plugins"
        }

   .. hie:command:: commands

      list available commands for a given plugin

      Example::

        {
            "params": {
                "plugin": {
                    "text": "the plugin name"
                }
            },
            "cmd": "base:commands"
        }

   .. hie:command:: commandDetail

      list parameters required for a given command

      Example::

        {
            "params": {
                "command": {
                    "text": "the command name"
                },
                "plugin": {
                    "text": "the plugin name"
                }
            },
            "cmd": "base:commandDetail"
        }

