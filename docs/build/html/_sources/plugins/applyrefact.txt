applyrefact
===========
.. hie:plugin:: applyrefact

   .. hie:command:: applyOne

      Apply a single hint

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
            "cmd": "applyrefact:applyOne"
        }

   .. hie:command:: applyAll

      Apply all hints to the file

      Example::

        {
            "params": {
                "file": {
                    "file": "a file name"
                }
            },
            "cmd": "applyrefact:applyAll"
        }

