
[![Build Status](https://travis-ci.org/stoltene2/personal-blog.svg)](https://travis-ci.org/stoltene2/chatty-lambdapersonal-blog)

# Create image for tagging

```
stack  --no-docker-set-user --docker-persist --docker-container-name=temp exec bash
```

# Build Docker image

```
docker build --cpuset-cpus 0,1,2 -t stoltene2/blog .
```

# Building the application

```
> docker run -it --name blog -v $(pwd):/blog stoltene2/blog /bin/bash
> stack build
```

# Deploy application

## Configure amazon cli

```
> aws config
```

## Deploy

```
> stack exec personal-blog build
> stack exec personal-blog deploy
```
