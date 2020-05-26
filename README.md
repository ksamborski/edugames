# Educational games

The goal of this repository is to have a bunch of simple games for kids to learn.

## List of games

### written multiplication

Generates two numbers and checks user's input if it follows correctly the written multiplication method.
Demo is available here: 

- PL: [karolsamborski.com/gry/mnozenie-pisemne](https://karolsamborski.com/gry/mnozenie-pisemne/)
- EN: [karolsamborski.com/en/games/written-multiplication](https://karolsamborski.com/en/games/written-multiplication/)
- ES: [karolsamborski.com/es/juegos/multiplicacion-vertical](https://karolsamborski.com/es/juegos/multiplicacion-vertical/)

## Build instructions

Running `make` will compile the source code with the debugger.
`make multiplication_release` will compile it with optimizations.

### Translation changes

When you add new or delete old translations you need to regenerate the code by running:
`make lang LANG=pl`

Please note that only changes in json file structure need code regeneration.

In order to see the translation you should pass `lang` parameter in the URL, e. g.: [localhost:8000/demo/multiplication.html?lang=es](http://localhost:8000/demo/multiplication.html?lang=es)
will show Spanish version of the game.

## Running

If you'd like to run the game locally you first need to compile it (using the steps above) and then run:
`docker-compose up`. After that you will be able to see it under [localhost:8000/demo/multiplication.html](http://localhost:8000/demo/multiplication.html)
