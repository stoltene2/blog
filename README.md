
[![Build Status](https://travis-ci.org/stoltene2/personal-blog.svg)](https://travis-ci.org/stoltene2/personal-blog)

# Building the personal-blog application

```
$ nix-build
# or
$ nix-shell
$ nix-build
```

# Deploying the blog

## Configure amazon cli credentials

```
$ nix-shell
$ aws configure
```

## Deploy

```
$ nix-shell
$ personal-blog build
$ personal-blog check
$ personal-blog deploy
```
