# bowling

This is a REST api implementation of a scoring application for a standard ten-pin bowling game. It supports:

* Generating a new scorecard for a game
* Scoring a Frame of the game
* Calculating the score of a given scorecard

## Installation Instructions

The application uses [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to build, run tests and run. You could also use this [Dockerfile](https://hub.docker.com/_/haskell) to build the application if you do not have stack available.

## Commands

Once you have stack installed:

* tests - You can run the tests by executing `stack test`.
* application - You can run the application by executing `stack run`
  * the application will run on port 3000
  * `POST /scorecards` will return you a new scorecard
  * `POST /scorecards/calculate` will calculate the score of the scorecard. If the game is complete it will give you the final score of the game as well
