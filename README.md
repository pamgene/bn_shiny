#BN Shiny server

## Install

```
devtools::install_bitbucket("bnoperator/bn_shiny")
```

# Publish a package on pamagene R repository

```
git add -A && git commit -m "++" && git push && git tag -a 2.22 -m "test" && git push --tags
```

```
bntools::deployGitPackage('https://bitbucket.org/bnoperator/bn_shiny.git', '2.22')
```