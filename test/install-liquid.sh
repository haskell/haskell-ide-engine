#!/bin/sh
if [ ! -f $HOME/.local/bin/liquid ]; then
    curl -o ~/.local/bin/liquid lukelau.me/hie/liquid	
fi
if [ ! -f $HOME/.local/bin/fixpoint ]; then
    curl -o ~/.local/bin/fixpoint lukelau.me/hie/fixpoint
fi
