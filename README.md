# misskey-rss-bot

## Execute

* Run `stack exec -- misskey-rss-bot-exe -c config` to see "We're inside the application!"
* With `stack exec -- misskey-rss-bot-exe --verbose -c config` you will see the same message, with more logging.

## Run tests

`stack test`

## Development
Run the following commands within the toolbox.

Run ghcup tui to install different versions of Haskell tools
### Build
`stack build`
`stack build --file-watch`
### Run
`stack exec -- misskey-rss-bot-exe -c config.yaml`
### Test
`stack test`
### All together
`stack build --test --file-watch --exec "stack exec ## Development
Run the following commands within the toolbox.

Run ghcup tui to install different versions of Haskell tools
### Build
`stack build`
`stack build --file-watch`
### Run
`stack exec -- misskey-rss-bot-exe -c config.yaml`
### Test
`stack test`
### All together
`stack build --test --file-watch --exec "stack exec -- misskey-rss-bot-exe -c config.yaml"`

## Formatting
`stack install stylish-haskell`
`stylish-haskell -i src/**`"`

# Dependencies
```bash
sudo dnf install -y gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs xz perl ncurses-devel ncurses-devel zlib-devel # Fedora packages required for GHCup/Stack

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
