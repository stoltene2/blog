
[![Build Status](https://travis-ci.org/stoltene2/blog.svg)](https://travis-ci.org/stoltene2/blog)

# Building the personal-blog application

```
$ nix-shell
$ nix-env -i pngquant awscli
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

# Updating friday or friday-devil

Most commonly I find that these package dependencies are out of
whack. To get around this I bump the versions in my fork. Then I
update the `friday.nix` file by using the following application from
inside my checkout.

```shell
nix-env -i nix-prefetch-git

nix-prefetch-git --output /tmp/friday --url git@github.com:stoltene2/friday.git --rev $(git log -1 --pretty=format:%H) --no-deepClone
...
{
  "url": "git@github.com:stoltene2/friday.git",
  "rev": "810ad3f87df93e8c883f5c4a2266df5aa8452826",
  "date": "2019-08-01T18:32:06+02:00",
  "sha256": "1v5x9jynm3h1zf501ff8rk8ah8gpc9p7n9j2pr3ral25y1y6hvyb",
  "fetchSubmodules": false
}

```
