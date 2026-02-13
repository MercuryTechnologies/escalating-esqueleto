# Escalating Esqueleto

[Esqueleto](https://hackage.haskell.org/package/esqueleto) is a library that lets us write SQL queries with Haskell syntax and typechecking.

This repo is a collection of Esqueleto exercises. They come with explanatory comments, a test suite, and sample answers.

Our goal is to be able to see through any Esqueleto query to the SQL underneath. Call it esq-ray vision.

## Getting Started

You'll need to have the [Nix](https://nixos.org/download/) package manager installed for `nix develop` to work.

### Database setup

Set up a local database:

```sh
nix develop
make db-create
```

You can reset database state (including repopulating it with fake data) with:

```
make db-reset
```

### Doing the exercises

The first exercise file is `exercises/EE0_StartingOut.hs`

1. Run `nix develop` to enter a nix shell
2. In the nix shell, run `make ghciwatch` to start the feedback loop
3. Edit `test/Main.hs` to focus on the relevant tests (see below)
4. Fix up the exercise file by filling in any typed holes or missing pieces
5. Save the file; `ghciwatch` will automatically reload and rerun the relevant tests

Then, move on to the next exercise file in `exercises/`

### Focusing on specific tests

`test/Main.hs` is spoiler-free

If you change `describe` to `fdescribe` in `test/Main.hs`, you can focus on a single file. Try it out:

```diff
- describe "EE0_StartingOut" $ do
+ fdescribe "EE0_StartingOut" $ do
```

Similarly, if you change `it` to `fit` you can focus on a single test.

### Viewing rendered queries

Because this repo's version of `runDB` logs to stdout, your queries can be seen in generated SQL form by looking for the `[Debug#SQL]` tag in the logs. It's often useful to check your intuition against the SQL your code is actually producing.

```
EE0_Test
[Debug#SQL] SELECT "test"."column"
FROM "test"
; []
  testName [✔]
  anotherTest [✘]
```

### Hints

There are hints for the exercises in the `hints/` directory, if you get stuck.

You could also read these for additional commentary after completing an exercise.

### Poking around in the database

Once setup is complete, you can check out the current state of the database with `psql`. This might be useful to write or validate raw SQL queries, but I encourage you to start thinking in Esqueleto directly rather than always translating.

```
make psql
```

### Check your work

:warning: `answers/*.hs` contains spoilers

Once you've completed an exercise, you may want to check against an existing answer. These are located in the `answers/` directory. The `answers/` files often contain additional commentary, such as pointing out footguns. To minimize spoilers, they're separated into one file per exercise.
