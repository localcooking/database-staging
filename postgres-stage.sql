-- This file is to be run _once_ per iteration of a postgresql server, not per database.

CREATE ROLE authenticator noinherit LOGIN PASSWORD 'foobar';

CREATE ROLE anon nologin;
CREATE ROLE customer nologin;
CREATE ROLE chef nologin;
CREATE ROLE moderator nologin;

GRANT anon TO customer;
GRANT anon TO chef;
GRANT anon TO moderator;
GRANT anon TO authenticator;

GRANT customer TO chef;
GRANT customer TO moderator;
GRANT customer TO authenticator;

GRANT chef TO moderator;
GRANT chef TO authenticator;

GRANT moderator TO authenticator;
