Local Cooking Database
===========================

Staging
---------

This package defines how to do the following:

- Set up a _new database_ with respect to the current version tag
- Migrate an _existing database_ the previous immediate version tag to the current version tag

### New Database

Using the `generate_database.sql` file, we can create a new database schema, without any data added to it. The only
thing we need to supply is a name for the consolidated schema - this way, migrating all of the data will be done
strictly though _copying_, and not mutation, eliminating the chance that data is lost due to poor engineering
of the new database, or the migration.

### Migrating Data

The `staging` command built by this package lets us move the data from the previous version to the new one.
Here is how it's done:

```bash
> staging --old-db-conn user@database.com:port --old-db-schema schema-name --new-db-conn user@database_new.com:port --new-db-schema schema-name-new --old-version v0.0.0 --new-version v0.0.1
```

This way, we can still move data on the same database connection (just duplicating the connection information),
or we can move it to a new host entirely. Likewise, we can supply version information to the command, to better
compose the migrations (granted, there may be a circumstance where data may be supplied per-entry).
