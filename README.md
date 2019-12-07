# Dependencies

    npm install -g elm elm-test elm-live tailwindcss

# Rebuild styling

    npx tailwind build src/styles.css -o dist/styles.css

# Run dev locally

    elm-live src/Main.elm -- --output=dist/adventofcode2019.js --debug
