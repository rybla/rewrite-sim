build:
    bun run build

bundle: build
    bun run bundle

serve: bundle
     http-server dist
