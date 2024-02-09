_WARNING_ For the moment you need to manually bump the npm packages :

    git checkout main
    git fetch origin main
    git describe --tags

    export VERSION=x.y.z
    ( cd facade; ./bump-version.js; yarn install )
    git diff

    git commit -am "bump facades to $VERSION"

    git tag -m 'release $VERSION' "v$VERSION"

    git push origin "v$VERSION"
    git push origin main
