*WARNING* For the moment you need to manually bump the npm packages :

git fetch origin main develop
    git describe
    export VERSION=x.y.z
    cd facade
    node bump-version.js
    git diff
    git ci -am "bump facades to $VERSION"