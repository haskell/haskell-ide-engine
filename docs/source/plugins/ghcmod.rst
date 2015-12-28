ghcmod
======
.. hie:plugin:: ghcmod

   .. hie:command:: check

      check a file for GHC warnings and errors

      Example::

        {
            "params": {
                "file": {
                    "file": "a file name"
                }
            },
            "cmd": "ghcmod:check"
        }

   .. hie:command:: lint

      Check files using ``hlint``

      Example::

        {
            "params": {
                "file": {
                    "file": "a file name"
                }
            },
            "cmd": "ghcmod:lint"
        }

   .. hie:command:: find

      List all modules that define SYMBOL

      Example::

        {
            "params": {
                "dir": {
                    "file": "a directory name"
                },
                "symbol": {
                    "text": "The SYMBOL to look up"
                }
            },
            "cmd": "ghcmod:find"
        }

   .. hie:command:: info

      Look up an identifier in the context of FILE (like ghci's ``:info``)

      Example::

        {
            "params": {
                "expr": {
                    "text": "The EXPR to provide info on"
                },
                "file": {
                    "file": "a file name"
                }
            },
            "cmd": "ghcmod:info"
        }

   .. hie:command:: type

      Get the type of the expression under (LINE,COL)

      Example::

        {
            "params": {
                "start_pos": {
                    "line": 42,
                    "col": 23
                },
                "file": {
                    "file": "a file name"
                }
            },
            "cmd": "ghcmod:type"
        }

