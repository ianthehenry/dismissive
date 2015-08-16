# Dismissive

Dismissive is a simple reminder service, and an experiment in implementing a traditional server as a suite of independent processes.

# Installing

## Services

Dismissive is a suite of independent services that all share a common [core library](./core). Each service needs a reference to `dismissive-core`, so there's a convenient `init` script provided that will set up sandboxes and install each service's dependencies. It will also install symlinks to the shared `conf` file, which is a convenience so that there's only one place to configure `dismissive-core`.

At some point, there will be `systemd` unit files here too.

## Database

Dismissive uses PostgreSQL. Create a database called `dismissive` and load up the schema:

    $ createdb dismissive
    $ psql dismissive -f ./schema.sql

Get your permission ducks in a row and edit `conf` with the connection string you want to use.

There is currently no schema migration story, as it's still in heavy flux. Your data is not safe.
