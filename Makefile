
code_files="$(find ./src ./app ./test -type f | grep .hs)"

build-only:
	stack build

build:
	stack build --pedantic --test

test:
	stack test

run: 
	stack exec -- misskey-rss-bot-exe -c config/default.yaml

dev-watch:
	stack build --fast --file-watch

repl:
	stack ghci

docker-build:
	podman-host build -t misskey-rss-bot .

docker-run:
	podman-host run -it misskey-rss-bot

format:
	stack exec -- stylish-haskell -i $(wildcard **/*.hs)

# build documentation (output in .stack-work/dist/x86_64-linux/Cabal-3.6.3.0/doc)
docs:
	stack haddock
