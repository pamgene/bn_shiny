#BN Shiny server

## Install

```
devtools::install_bitbucket("bnoperator/bn_shiny")
```

# Publish a package on pamagene R repository

```
git add -A && git commit -m "isInstalled not correctly called" && git push && git tag -a 2.23 -m "++" && git push --tags
```

```
bntools::deployGitPackage('https://bitbucket.org/bnoperator/bn_shiny.git', '2.23')
```