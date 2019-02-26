# A record, from name to path, of the third-party packages
with
{
  versions = builtins.fromJSON (builtins.readFile ./sources.json);

  # fetchTarball version that is compatible between all the versions of Nix
  fetchTarball =
    { url, sha256 }:
      if builtins.lessThan builtins.nixVersion "1.12" then
        builtins.fetchTarball { inherit url; }
      else
        builtins.fetchTarball { inherit url sha256; };
};

# NOTE: spec must _not_ have an "outPath" attribute
builtins.mapAttrs (_: spec:
  if builtins.hasAttr "outPath" spec
  then abort
    "The values in versions.json should not have an 'outPath' attribute"
  else
    if builtins.hasAttr "url" spec && builtins.hasAttr "sha256" spec
    then
      spec //
    { outPath = fetchTarball { inherit (spec) url sha256; } ; }
    else spec
  ) versions
