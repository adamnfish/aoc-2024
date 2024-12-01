# aoc-2024

My solutions to Advent of Code 2024.

## Running locally

The program can be executed using `sbt`.

### Solving a day

Provide the day, the puzzle number, and the input file to read (typically `example` or `input`)

```shell
$ sbt
> run 1 1 input
```

### Loading puzzle input

You can also load your puzzle input for the days challenge, It will be saved into the correct location.

This requires an Advent of Code auth token, which you can get from the Advent of Code website. Look at the cookies for site, you're after the value of the cookie called `session`.

The Advent of Code auth token is expect in an environment variable called `AOC_SESSION`.

Provide the string `load` as the first argument, and then the second argument is the puzzle number to load.

```shell
$ AOC_SESSION=abc12345678910 sbt
> run load 1
```

## Run tests

The tests can be run using `sbt`.

```shell
$ sbt
> test
```
