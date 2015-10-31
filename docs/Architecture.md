## Overall Architecture
```
                    +-----+                      |
                    | IDE |                      |
          (1)       +o-o-o+     (1)              |
           ---------´  |  `-------               +------------------
          /            |          \              |
+--------o-----------+ | +--------o-----------+  | client connects to the daemon
| haskell-ide-client | | | haskell-ide-client |  | and provides the channel
+--------o-----------+ | +--------o-----------+  | abstraction to the IDE.
          \            |         /               |
       (1) -----    (2)|    ----- (2)            +------------------
                `      |   ´                     |
            +---o------o---o---+                 | daemon hosts plugins and
            |haskell-ide-daemon|                 | dispatches messages to them
            +---o----oo----o---+                 |
               /     ||     \                    +------------------
+-------------o---+  ||  +---o--------------+    |
|haskell-ide-tools|  ||  |(haskell-ide-)ghci|    | plugins/tools provide the
+-----------------+  ||  +------------------+    | functionality and communicate
                    /  \                         | with IDE directly OR via our
               +----+ +----+                     | "unified" API (TBD)
               |HaRe| |mote|                     |
               +----+ +----+                     |

(1): Channel abstraction haskell-ide-client provides for the IDE
(2): Direct multiplexed connection to daemon
```
