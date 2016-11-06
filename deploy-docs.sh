#!/bin/bash

virtualenv env
. env/bin/activate
pip install -U Sphinx

set -e # exit with nonzero exit code if anything fails

# build docs generator and create docs
stack exec hie-docs-generator -- --prefix docs/source/

# run our compile script, discussed above
cd docs
make clean

# clone gh-pages remote repo before building docs with latest changes
# remove all files but .git
git clone --quiet --branch gh-pages --single-branch "https://${GH_TOKEN}@${GH_REF}" build/html
cd build/html && git rm -r -f --quiet -- '*' && cd ../..

make html

# disable for pull requests
if [ "$TRAVIS_PULL_REQUEST" != "false" ]
then
    echo "Exiting: in a pull request"
    exit 0
fi

# disable for other branches than master
if [ "$TRAVIS_BRANCH" != "master" ]
then
    echo "Exiting: not on master branch"
    exit 0
fi

# disable for other repos as it will fail anyway because the
# encryption is repo specific
if [ "$TRAVIS_REPO_SLUG" != "haskell/haskell-ide-engine" ]
then
    echo "Exiting: not on haskell/haskell-ide-engine repo"
    exit 0
fi

# go to the out directory where there is Git repo cloned from above and
# output from `make html` command
cd build/html

touch .nojekyll

# inside this git repo we'll pretend to be a new user
git config user.name "Travis CI"
git config user.email "moritz.kiefer@purelyfunctional.org"

# The first and only commit to this local cloned Git repo contains all the
# files present with the commit message "Deploy to GitHub Pages".
git add .
git commit -m "Deploy to GitHub Pages" --allow-empty

# Push from the current repo's gh-pages branch to the remote
# repo's gh-pages branch.
git push --quiet "https://${GH_TOKEN}@${GH_REF}" gh-pages:gh-pages
