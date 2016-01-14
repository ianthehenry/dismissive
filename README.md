# Dismissive

Dismissive is a simple reminder service.

More than that, it's a chance to play with and learn about:

- implementing an application as a suite of independent processes in Haskell
- [`servant`](https://hackage.haskell.org/package/servant)
- [`persistent`](https://hackage.haskell.org/package/persistent)

It is very much a work in progress. There are small fragments of it that happen work, but it is not yet a *usable* piece of software.

# Installing

## Services

If you use `stack`, everything is trivial and works beautifully out of the box.

## Database

Dismissive uses PostgreSQL, so get that installed and running.

Then create a database and load up the schema:

    $ createdb dismissive
    $ psql dismissive -f ./schema.sql

Get your permission ducks in a row and edit `conf` with the connection string you want to use.

There is currently no schema migration story, as it's still in heavy flux. Your data is not safe here.
