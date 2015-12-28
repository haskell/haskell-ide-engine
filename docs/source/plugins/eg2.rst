eg2
===
.. hie:plugin:: eg2

   .. hie:command:: sayHello

      say hello

      Example::

        {
            "params": {},
            "cmd": "eg2:sayHello"
        }

   .. hie:command:: sayHelloTo

      say hello to the passed in param

      Example::

        {
            "params": {
                "name": {
                    "text": "the name to greet"
                }
            },
            "cmd": "eg2:sayHelloTo"
        }

