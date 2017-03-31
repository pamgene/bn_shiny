#BN Shiny server

## Install

```
devtools::install_bitbucket("bnoperator/bn_shiny")
```

# Publish a package on pamagene R repository

```
git add -A && git commit -m "add support for shiny app with no input" && git push && git tag -a 2.24 -m "++" && git push --tags
```

```
bntools::deployGitPackage('https://bitbucket.org/bnoperator/bn_shiny.git', '2.24')
```