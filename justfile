build:
  bun spago build

bundle: build
  bun spago bundle --module Main --outfile ./dist/main.js

serve: bundle
  bun http-server dist

deploy: bundle
  bun gh-pages --dist dist --nojekyll
