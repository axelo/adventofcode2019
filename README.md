# Dependencies

Node version >= 8

    npm install -g elm elm-test

# Build

    elm make src/Headless.elm --optimize --output=dist/headless.js

# Running

    node Headless dayToSolve [otherDayToSolve] [andAnotherDayToSolve]

## Examples

    node Headless 1

Will try to solve day 1.

    node Headless 1 2

Will try to solve day 1 and day 2.

# Test

    elm test
