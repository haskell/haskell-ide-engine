## How to use hie with Leksah

This document is a work in progress, as is the Leksah integration.

### Getting started

```bash
git clone https://github.com/haskell/haskell-ide-engine.git
cd haskell-ide-engine
stack install
```

This will put the `hie` executable into `$HOME/.local/bin/hie`.

Get Leksah sources from the hie-integration branch (https://github.com/Leksah/Leksah/tree/hie_integration) and build them.
This branch has a dependency on hie-base, one of the submodules of haskell-ide-engine. So one way to build things is to add the directory where you cloned the hie-base sources to the Leksah sandbox or stack file.

Once Leksah is built, you can start it normally. Then go to `Tools -> Preferences -> Helper Programs` and check the `use haskell-ide-engine` box, and fill in the path to the executable (`$HOME/.local/bin/hie`).

### Usage

Once the haskell-ide-engine path set in the preferences, the following actions are available:

- Type: will use the `ghcmod:type` command to display the type of the selected expression (no need to have a GHCi session running in Leksah)
- Info: will use the `ghcmod:info` command to display information of the selected expression
- Refactor: a few refactorings are available, provided by the hie HaRe plugin. Some like rename or dupdef will ask for the new name. Running the refactoring currently replaces the whole source contents.

### Troubleshooting

Some messages are dumped to the Leksah log, so run Leksah in debug mode to see them.
