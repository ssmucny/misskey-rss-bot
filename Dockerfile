FROM haskell:9.2.6

# Copy src files
COPY . /app
WORKDIR /app

# Build
RUN stack setup && stack build --only-dependencies
RUN stack build --pedantic --test

# Start application
CMD stack exec -- misskey-rss-bot-exe --config ./config/default.yaml