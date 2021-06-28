--[[ users pendingRegistrations sessions chefs menus items carts orders reviews ]]
--{{ users pendingRegistrations activePendingRegistrations sessions activeSessions chefs credentials chefCredentials menus items itemRevisions latestActiveItemRevisions menuItemMapping carts orders orderContents reviews }}

CREATE EXTENSION IF NOT EXISTS pgcrypto;
-- CREATE EXTENSION IF NOT EXISTS timescaledb;

CREATE SCHEMA api;

-- CREATE ROLE anon nologin;

GRANT USAGE ON SCHEMA api TO anon;

-- CREATE ROLE authenticator noinherit LOGIN PASSWORD 'foobar';
-- GRANT anon TO authenticator;

--[[ users ]]

--[[ pendingRegistrations ]]

--[[ sessions ]]

--[[ chefs ]]

--[[ menus ]]

--[[ items ]]

--[[ carts ]]

--[[ orders ]]

--[[ reviews ]]
