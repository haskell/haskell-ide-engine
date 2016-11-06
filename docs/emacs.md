## How to use hie with emacs

This document is a work in progress, as is the emacs integration.

The current version can invoke commands, but does not do anything useful with the replies.

### Getting started

```bash
git clone https://github.com/haskell/haskell-ide-engine.git
cd haskell-ide-engine
stack install
```

This will put the `hie` executable into `$HOME/.local/bin/hie`. Make sure this is on your path.

Add the following to your `.emacs`, updating the paths to reflect the location the project was cloned to.

```emacs
(add-to-list 'load-path "/home/alanz/mysrc/github/haskell/haskell-ide-engine/elisp")
(load-file  "/home/alanz/mysrc/github/haskell/haskell-ide-engine/elisp/hie.el")
```

Start up emacs, and open a haskell file.

Make sure the menu bar is visible, by

    M-x menu-bar-mode

to toggle it.

Turn on `hie-mode`, which is a minor mode

    M-x hie-mode

After a few seconds, a new section should occur in the menu bar, for `HIE`. If not, resize the window, or click in the window area somewhere. This seems to be a bug, that it does not display initially.

The `HIE` menu lists the various plugins, and the commands exposed by each. which can be activated from the menu.

Each command is also made available as a function with a name of the form

    hie-PLUGIN-COMMAND

So the `demote` command exposed by the `hare` plugin shows up as

    hie-hare-demote

Note that the menu and functions are created based on interrogation of the `hie` executable, so if for some reason it does not start up the menu will be empty.

### Customisation

There are two options that can be customized, by invoking `M-x customize` and then searching for `hie-command`

    hie-command
    hie-command-args

The first one sets the executable to run for `hie`, set the full path for it if it is not normally in your path.

The second can be used to pass arguments to the executable, the most useful being to put it into debug mode and send its output to a log file.  This can be done by adding three separate values to it

    -d
    -l
    /tmp/hie.log

The first enables debug logging, rather than just info logging.
The next two specify that log output should go to a file, and what the file is.

### emacs logging

When `hie-mode` starts up it creates a buffer called `*hie-log*` which keeps a record of the JSON-encoded messages exchanged between emacs and hie.
