# Haskell IDE Engine Specific Configuration

## File format and options 
> `hie` will look for the `hie.yaml` file in `/$PWD/hie.yaml` or `/$HOME/hie.yaml`.

```yaml
overrides:
  # Disables interactive “as you type“ linter/diagnostic feedback.
  - on_save_only
  # Excludes argument types from autocomplete insertions.
  - no_autocomplete_arguments
```


### With regards to atom users:
* If using the ‘linter’ package, setting “Lint on Change” to `false` will have no effect unless you create an `hie.yaml` file with the `on_save_only` option.
* Completion insertions from the ‘linter’ or the ‘atom-ide-ui’ packages in conjunction with 'hie' and 'ide-haskell-hie' will include the argument types. E.g. selecting `mapM` will insert `mapM a -> m b t a` unless your `hie.yaml` file includes the `no_autocomplete_arguments` option.


