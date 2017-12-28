FROM fpco/stack-build:lts-9.6

RUN apt-get update
RUN apt-get install -y libdevil-dev pngquant awscli

RUN stack setup --resolver lts-9.6
ADD stack.yaml personal-blog.cabal ./

RUN stack build --only-dependencies --allow-different-user

EXPOSE 8000

VOLUME /blog

WORKDIR /blog