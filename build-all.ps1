# Windows PowerShell script that replicates the function of 'make build-all'.
#
# The Makefile itself cannot be used on Windows, because it contains certain
# UNIX-only features, such as `cp` command and extensionless executables.

$stackbindir = stack path --local-bin

$supported_versions =
    @{ "8.2.1" = "8.2"
    ;  "8.2.2" = "8.2"
    ;  "8.4.2" = "8.4"
    ;  "8.4.3" = "8.4"
    ;  "8.4.4" = "8.4"
    ;  "8.6.1" = "8.6"
    ;  "8.6.2" = "8.6"
    ;  "8.6.3" = "8.6"
    }

function main($requested_versions) {
    $build_versions =
        $requested_versions | sort `
        | %{
            $major = $supported_versions[$_]
            if (-not $major) { bail_unsupported_version $_ }
            @{ version=$_; major=$major }
        }

    echo "Building HIE for GHC versions: $($build_versions | %{ $_.version } | sort)"
    git submodule update --init
    $build_versions | %{ build $_.version $_.major }
}

function build($version, $major) {
    echo "Building HIE for GHC $version"
    stack --stack-yaml=stack-$version.yaml install happy
    stack --stack-yaml=stack-$version.yaml install
    cp "$stackbindir\hie.exe" "$stackbindir\hie-$version.exe"
    cp "$stackbindir\hie.exe" "$stackbindir\hie-$major.exe"

    echo "Building Hoogle database for GHC $version"
    stack --stack-yaml=stack-$version.yaml exec hoogle generate
}

function bail_unsupported_version($v) {
    echo "GHC $v is not supported."
    echo "Specify list of GHC versions to build for, or omit any arguments to build for all supported versions."
    echo "Supported versions are: $($supported_versions.Keys)"
    exit
}

if ($args) { main $args }
else { main $supported_versions.Keys }
