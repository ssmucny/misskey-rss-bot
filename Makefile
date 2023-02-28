build-only:
	stack build

build:
	stack build --pedantic --test

run: build
	stack exec -- misskey-rss-bot-exe

dev-watch:
	stack build --fast --test --file-watch

docker-build:
	podman-host build -t misskey-rss-bot .

docker-run:
	podman-host run -it misskey-rss-bot

format:
	stack exec -- stylish-haskell -i src/** app/** test/**