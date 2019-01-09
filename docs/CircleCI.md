# CircleCI

## Jobs

For each version of GHC supported, CircleCI will kick off a build that will build and test HIE for that version.
Whenever a new stack-x.y.z is added, be sure to also add it to both the `workflow` and `jobs` sections of `config.yml`.

In addition to the stack based jobs, there is also a job that builds HIE with `cabal new-build`. 
Currently tests are not run as there is an issue with cabal-helper-wrapper that prevents any ghc-mod tests from passing.

## Docker

Each job is carried out in a Docker container built from https://github.com/alanz/haskell-dockerfiles/blob/master/haskell-hie-ci/Dockerfile

## Caching

Since HIE takes a long time to build, CI caches things very liberally.
Most importantly, the `~/.stack-work` directories for HIE as well as every submodule are cached before and after testing (This is so that if the build passes but the tests fail there is still something cached for the next build). 

### Change detection 

Before restoring any cache, the CircleCI job creates three files and uses their checksum to detect any changes:

1. `all-cabal.txt` detects any changes to `.cabal` files (not in submodules or test data)
2. `stack-build.txt` detects any changes to the `stack-x.y.z` file
3. `resolver.txt` detects any changes to the stack resolver

Each job will tries to restore the most specific cache avaialble for it, and fallback to a more general cache if not available:


```yaml
- restore_cache:
    keys:
        - stack-cache-{{ .Environment.HIE_CACHE }}-{{ arch }}-{{ .Environment.CIRCLE_JOB }}-{{ checksum "stack-build.txt" }}-{{ checksum "all-cabal.txt" }}
        - stack-cache-{{ .Environment.HIE_CACHE }}-{{ arch }}-{{ .Environment.CIRCLE_JOB }}-{{ checksum "stack-build.txt" }}
        - stack-cache-{{ .Environment.HIE_CACHE }}-{{ arch }}-{{ .Environment.CIRCLE_JOB }}-{{ checksum "resolver.txt" }}
```

## Invalidating the cache

If you need to clear the cache on CircleCI, bump the `$HIE_CACHE` environment variable via the CircleCI web interface and rerun the workflow. 
