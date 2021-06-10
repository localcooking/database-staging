--{{ users pendingRegistrations activePendingRegistrations sessions activeSessions chefs credentials chefCredentials menus items itemRevisions latestActiveItemRevisions menuItemMapping carts orders orderContents reviews }}

CREATE EXTENSION pgcrypto;

/*

--{{ users }}

*/
CREATE TABLE IF NOT EXISTS users (
  user_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  email VARCHAR ( 255 ) UNIQUE NOT NULL,
  password BYTEA NOT NULL CHECK (octet_length(password) = 64),
  salt BYTEA NOT NULL CHECK (octet_length(salt) = 32),
  created_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  last_login TIMESTAMPTZ,
  last_active TIMESTAMPTZ,
  deactivated_on TIMESTAMPTZ,
  CONSTRAINT chk_login_created CHECK (created_on <= last_login) -- sanity-check
  /* TODO payment methods */
  /* TODO shipping & billing */
);

comment on table users is 'All users that can log-in';
comment on column users.password is 'Argon2 hashed output';
comment on column users.salt is 'Unique salt per-user';
comment on column users.deactivated_on is 'Allows a user to pseudo-delete their account without our record-books being corrupted';

CREATE OR REPLACE FUNCTION deactivate_user(user_id_ INT) RETURNS void AS
$$
BEGIN
  UPDATE users SET deactivated_on = CURRENT_TIMESTAMP WHERE user_id = user_id_ RETURNING deactivated_on;
  DELETE FROM sessions WHERE user_id = user_id_; -- Revoke sessions
END;
$$
  LANGUAGE plpgsql
  VOLATILE;

CREATE VIEW active_users AS
  SELECT * FROM users WHERE deactivated_on IS NULL;


/*
Here, during registration, we email the address a link with a unique token,
to verify the email is authentic. Furthermore, in the email with a "click here to complete"
link, we'll also include everything they put in the registration form as url parameters
(pkdf2 encrypting the password first).

--{{ pendingRegistrations }}

*/
CREATE TABLE IF NOT EXISTS pending_registrations (
  email VARCHAR ( 255 ) UNIQUE NOT NULL, -- Doesn't reference users because it doesn't exist yet.
  auth_token uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  expiration TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP + INTERVAL '1 day'
);

comment on table pending_registrations is 'Any pending registrations that need to click the email link';

-- TODO call from a cron job
CREATE OR REPLACE FUNCTION expire_pending_registrations() RETURNS void AS
$$
  DELETE FROM pending_registrations WHERE expiration < CURRENT_TIMESTAMP;
$$
  LANGUAGE SQL
  VOLATILE;

/*
Table of only active pending registrations as a view, to make things easier and less reliant on cron-jobs
to ensure pending registrations are legitimate.

  --{{ activePendingRegistrations }}

*/
CREATE VIEW active_pending_registrations AS
  SELECT * FROM pending_registrations WHERE expiration >= CURRENT_TIMESTAMP;

comment on view active_pending_registrations is 'Any pending registrations that are not expired';

CREATE OR REPLACE FUNCTION assign_registration(email_ VARCHAR) RETURNS uuid AS
$$
  -- Find an active user with the same email
  INSERT INTO pending_registrations (email) SELECT $1 WHERE NOT EXISTS
    (SELECT 1 FROM users WHERE email = email_ AND deactivated_on IS NULL)
    RETURNING auth_token;
$$
  LANGUAGE SQL
  VOLATILE
  RETURNS NULL ON NULL INPUT;

CREATE OR REPLACE FUNCTION register(registration_ uuid, email_ VARCHAR, password_ BYTEA, salt_ BYTEA) RETURNS INT AS
$$
DECLARE
  uid INT;
  is_pending INT;
BEGIN
  -- Ensure a valid pending registration exists
  SELECT 1 INTO is_pending FROM pending_registrations
    WHERE email = email_
      AND auth_token = registration_
      AND expiration >= CURRENT_TIMESTAMP;

  IF is_pending = 1 THEN
    -- delete it
    DELETE FROM pending_registrations
    WHERE auth_token = registration_
    AND expiration >= CURRENT_TIMESTAMP
    AND email = email_;

    -- Is the user just deactivated?
    -- SELECT 1 FROM users WHERE email = email_;

    -- IF found THEN
    --   -- reactivate user
    --   UPDATE users SET deactivated_on = NULL WHERE email = email_;
    -- ELSE
      -- insert the info into the user's column
    INSERT INTO users (email, password, salt)
    VALUES (email_, password_, salt_)
    -- WHERE NOT EXISTS (SELECT 1 FROM users WHERE email = email_) -- Redundant
    -- ON CONFLICT DO UPDATE SET deactivated_on = NULL -- Don't make registration the same thing as reactivation
    -- WHERE email = email_ AND password = password_ AND salt = salt_ -- what if the password is incorrect?
    RETURNING user_id INTO uid;
    -- END IF;
    RETURN uid;
  ELSE
    RAISE 'Pending registration not found: %', is_pending;
    RETURN NULL;
  END IF;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;

/*
Every time a user successfully logs in, we'll issue them a session token that gets kept in a
cookie, and verified every time they check out a page on the website. Every time communication
is valid in a good session, the expiration resets to now + 1 day. Then as a cron job, the
database will prune old data during off hours.

--{{ sessions }}

*/
CREATE TABLE IF NOT EXISTS sessions (
  user_id INT NOT NULL REFERENCES users (user_id) ON DELETE RESTRICT, -- not unique - multiple logged-in users
  session_token uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  expiration TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP + INTERVAL '1 day'
  -- TODO when a user is pseudo-deleted via `inactive`, delete all session rows for that user, logging them out
);

comment on table sessions is 'Any active logged-in users, with their session token and expiration';

-- TODO call from a cron job
CREATE OR REPLACE FUNCTION expire_sessions() RETURNS void AS
$$
  DELETE FROM sessions WHERE expiration < CURRENT_TIMESTAMP;
$$
  LANGUAGE SQL
  VOLATILE;

-- TODO make a selection function that also updates its expiration when a row is found


/*
Table of only active sessions as a view, to make things easier and less reliant on cron-jobs to ensure
sessions are legitimate.

--{{ activeSessions }}

*/
CREATE VIEW active_sessions AS
  SELECT * FROM sessions WHERE expiration >= CURRENT_TIMESTAMP;

CREATE OR REPLACE FUNCTION get_login_salt(email_ VARCHAR) RETURNS BYTEA AS
$$
  SELECT salt FROM users WHERE email = email_;
$$
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT;

CREATE OR REPLACE FUNCTION login(email_ VARCHAR, password_ BYTEA) RETURNS uuid AS
$$
DECLARE
  user_id_ INT;
  active_session uuid;
BEGIN
  SELECT INTO user_id_ user_id FROM users WHERE email = email_ AND password = password_;
  UPDATE users SET last_login = CURRENT_TIMESTAMP, last_active = CURRENT_TIMESTAMP WHERE user_id = user_id_; -- update last login
  INSERT INTO sessions (user_id) VALUES (user_id_) RETURNING session_token INTO active_session; -- insert session
  RETURN active_session;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;

CREATE OR REPLACE FUNCTION touch_session(session_token_ uuid) RETURNS uuid AS
$$
DECLARE
  user_id_ INT;
  active_session uuid;
BEGIN
  SELECT INTO user_id_ user_id FROM sessions WHERE session_token = session_token_; -- find logged-in user
  UPDATE users SET last_active = CURRENT_TIMESTAMP WHERE user_id = user_id_; -- update last active
  UPDATE sessions SET session_token = gen_random_uuid()
    WHERE session_token = session_token_ RETURNING session_token INTO active_session;
  RETURN active_session;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;

CREATE OR REPLACE FUNCTION logout(session_token_ uuid) RETURNS INT AS
$$
  DELETE FROM sessions WHERE session_token = session_token_ RETURNING user_id;
$$
  LANGUAGE SQL
  VOLATILE
  RETURNS NULL ON NULL INPUT;

CREATE TYPE subscription_type AS ENUM ('free', 'premium', 'gold', 'platinum');

/*
A chef is a Local Cooking user that we vet to have the power to create their own menus.

--{{ chefs }}

*/
CREATE TABLE IF NOT EXISTS chefs (
  user_id INT UNIQUE NOT NULL REFERENCES users (user_id) ON DELETE RESTRICT,
  chef_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  public_name VARCHAR ( 255 ) NOT NULL,
  profile VARCHAR ( 1024 ),
  enrolled_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  revoked_on TIMESTAMPTZ, -- Pseudo-deletion
  subscription subscription_type NOT NULL,
  subscription_expiration TIMESTAMPTZ NOT NULL
  -- FIXME product delivery method
  -- TODO Payment method (same as user?)
  -- TODO chef rating?
);


CREATE OR REPLACE FUNCTION enroll(user_id_ INT, public_name_ VARCHAR, subscription_ subscription_type, subscription_expiration_ TIMESTAMPTZ) RETURNS INT AS
$$
  INSERT INTO chefs (user_id, public_name, subscription, subscription_expiration)
  VALUES (user_id_, public_name_, subscription_, subscription_expiration_) RETURNING chef_id;
$$
  LANGUAGE SQL
  VOLATILE
  RETURNS NULL ON NULL INPUT;

CREATE OR REPLACE FUNCTION disenroll(chef_id_ INT) RETURNS void AS
$$
BEGIN
  -- revoke chef
  UPDATE chefs SET revoked_on = CURRENT_TIMESTAMP WHERE chef_id = chef_id_;
  -- delete all the chef's items
  WITH chefs_items AS (
    SELECT items.item_id AS item_id
      FROM menu_item_mapping
          INNER JOIN menus
              ON menu_item_mapping.menu_id = menus.menu_id
          INNER JOIN items
              ON menu_item_mapping.item_id = items.item_id
      WHERE menus.chef_id = chef_id_
  )
  UPDATE items SET deleted_on = CURRENT_TIMESTAMP FROM chefs_items
  WHERE items.item_id = chefs_items.item_id;

END;
$$
  LANGUAGE plpgsql
  VOLATILE;

/*
At Local Cooking, we're going to grow our chef's credentials over time. Each new trend
will require new skills and certifications - for instance, being trained in gluten intolerance,
or peanut allergies, could be a specific trade. Furthermore, we may certify someone as a
vegan cook, which may grant them special interest. Likewise, in the future, chefs may issue
certifications of their own, which they use to vet other chefs and grow their academia.

--{{ credentials }}

*/
CREATE TABLE IF NOT EXISTS credentials (
  credential_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  title VARCHAR ( 255 ) NOT NULL,
  description VARCHAR ( 1024 ) NOT NULL
);

/*
Mapping between chefs and their credentials.

--{{ chefCredentials }}

*/
CREATE TABLE IF NOT EXISTS chef_credentials (
  chef_id INT NOT NULL REFERENCES chefs (chef_id) ON DELETE RESTRICT,
  credential_id INT NOT NULL REFERENCES credentials (credential_id) ON DELETE RESTRICT,
  UNIQUE (chef_id, credential_id) -- A chef can't have two of the same credential
);

CREATE OR REPLACE FUNCTION get_chef_credentials(chef_id_ INT) RETURNS record AS
$$
  SELECT credentials.credential_id, credentials.title, credentials.description
  AS credential_id, title, description
    FROM chef_credentials
         INNER JOIN credentials
             ON chef_credentials.credential_id = credentials.credential_id
    WHERE chef_credentials.chef_id = chef_id_;
$$
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT;


/*
Menus are the curated options that users can select to purchase orders from chefs.

--{{ menus }}

*/
CREATE TABLE IF NOT EXISTS menus (
  menu_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  chef_id INT NOT NULL REFERENCES chefs (chef_id) ON DELETE RESTRICT,
  title VARCHAR ( 255 ) NOT NULL,
  description VARCHAR ( 1024 ) NOT NULL
  -- TODO themes
  -- TODO images
);
/* TODO constraints - one menu for free, three for premium, 10 for gold, inf for platinum */

/*
Menu items are the actual products that users can purchase, which may be showcased in one or more
menu.

--{{ items }}

*/
CREATE TABLE IF NOT EXISTS items (
  item_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  deleted_on TIMESTAMPTZ, -- pseudo-deletion; doesn't actually delete from db for order book
  created_on TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  ratings INT NOT NULL DEFAULT 0,
  rating decimal CHECK (rating >= 1 AND rating <= 5) -- accumulated ratings, to not waste computation
);
/* TODO constraints - 5 for free, 15 for premium, 30 for gold, inf for platinum */

/*
Allows for revisions to be saved over time, that way an order refers to a specific edition of an item,
and not one that was edited after being ordered.

--{{ itemRevisions }}

*/
CREATE TABLE IF NOT EXISTS item_revisions (
  item_revision_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  item_id INT NOT NULL REFERENCES items (item_id) ON DELETE RESTRICT,
  title VARCHAR ( 255 ) NOT NULL,
  description VARCHAR ( 1024 ) NOT NULL,
  price INT NOT NULL CHECK (price > 0),
  appx_order_delay INTERVAL DAY TO HOUR NOT NULL,
  edited_on TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
  /* TODO images */
  /* FIXME categories like vegan etc. */
);
-- FIXME max number of revisions? Revision spamming?

/*
A simple sub-table where each item has it's latest revision information
attached.

--{{ latestActiveItemRevisions }}

*/
CREATE VIEW latest_active_item_revisions AS
  SELECT items.item_id,
         items.created_on,
         latest_revision.item_revision_id,
         latest_revision.title,
         latest_revision.description,
         latest_revision.price,
         latest_revision.appx_order_delay,
         latest_revision.edited_on
    FROM items
         JOIN (SELECT item_revisions.item_revision_id,
                      item_revisions.item_id,
                      item_revisions.title,
                      item_revisions.description,
                      item_revisions.price,
                      item_revisions.appx_order_delay,
                      MAX(item_revisions.edited_on) AS edited_on
                FROM item_revisions
                GROUP BY item_revisions.item_revision_id) latest_revision
                ON latest_revision.item_id = items.item_id
    WHERE items.deleted_on IS NULL;

/*
Many-to-many relationship of the same item going to different menus, and the same menu having different items.

--{{ menuItemMapping }}

*/
CREATE TABLE IF NOT EXISTS menu_item_mapping (
  menu_item_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  menu_id INT NOT NULL REFERENCES menus (menu_id) ON DELETE RESTRICT,
  item_id INT NOT NULL REFERENCES items (item_id) ON DELETE RESTRICT,
  absolute_position INT NOT NULL
  /* FIXME max number of items per menu */
);
/* TODO constraints - 5 for free, 15 for premium, 30 for gold, inf for platinum */
-- Nothing references this table, rows can be added & deleted freely per the chef's desire.

-- CREATE TABLE IF NOT EXISTS menu_item_ordering (
--   menu_id INT UNIQUE NOT NULL REFERENCES menus (menu_id) ON DELETE CASCADE,
--   menu_item_ids INT[] NOT NULL REFERENCES menu_item_mapping (menu_item_id) ON DELETE PRUNE -- wtf
-- );

-- TODO should I also get the latest version?
CREATE OR REPLACE FUNCTION get_menu_items(menu_id_ INT) RETURNS record AS
$$
  SELECT items.item_id, items.deleted_on, items.created_on
  AS item_id, deleted_on, created_on
    FROM menu_item_mapping
         INNER JOIN items
             ON menu_item_mapping.item_id = items.item_id
    WHERE menu_item_mapping.menu_id = menu_id_
    ORDER BY menu_item_mapping.absolute_position ASC;
$$
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT;

/*
The cart for people to file orders

--{{ carts }}

*/
CREATE TABLE IF NOT EXISTS carts (
  cart_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  user_id INT NOT NULL REFERENCES users (user_id) ON DELETE RESTRICT,
  item_id INT NOT NULL REFERENCES items (item_id) ON DELETE RESTRICT, -- menu_item_mapping is used only for views
  unseen_revision INT REFERENCES item_revisions (item_revision_id) ON DELETE RESTRICT, -- when creating a new revision, also set all carts to that revision as well.
  quantity INT NOT NULL DEFAULT 1 CHECK (quantity > 0),
  added_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (user_id, item_id) /* increment quantity instead - FIXME can I just update on insert / delete? */
);
-- When a user checks out, their cart rows are deleted - nothing references this table.

CREATE OR REPLACE FUNCTION get_cart(user_id_ INT) RETURNS record AS
$$
  SELECT carts.cart_id, carts.item_id, carts.unseen_revision, carts.quantity, carts.added_on
  AS cart_id, item_id, unseen_revision, quantity, added_on
  FROM carts
  WHERE user_id = user_id_
  ORDER BY added_on DESC; -- TODO pagination?
$$
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT;

/*
Record of all orders, after a user presses "checkout"

--{{ orders }}

*/
CREATE TABLE IF NOT EXISTS orders (
  order_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  user_id INT NOT NULL REFERENCES users (user_id) ON DELETE RESTRICT,
  ordered_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

/*
Each item purchased under a single order (after a user clicks "Checkout").

--{{ orderContents }}

*/
CREATE TABLE IF NOT EXISTS order_contents (
  order_content_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  order_id INT NOT NULL REFERENCES orders (order_id) ON DELETE RESTRICT,
  item_revision_id INT NOT NULL REFERENCES item_revisions (item_revision_id) ON DELETE RESTRICT,
  quantity INT NOT NULL CHECK (quantity > 0),
  completed_on TIMESTAMPTZ,
  delivered_on TIMESTAMPTZ,
  CONSTRAINT chk_completed_delivered CHECK (NOT (delivered_on IS NOT NULL AND completed_on IS NULL)) /* makes sure it's not delivered before it's complete */
);

/*
Reviews for a specific order's contents.

--{{ reviews }}

*/
CREATE TABLE IF NOT EXISTS reviews (
  review_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  order_content_id INT UNIQUE NOT NULL REFERENCES order_contents (order_content_id) ON DELETE RESTRICT, /* Only one review per order content */
  reviewed_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  title VARCHAR ( 255 ) NOT NULL,
  description VARCHAR ( 1024 ) NOT NULL,
  /* TODO images */
  stars decimal(2,1) CHECK (stars >= 1 AND stars <= 5) /* For aggregates */
);


CREATE OR REPLACE FUNCTION review(order_content_id_ INT, title_ VARCHAR, description_ VARCHAR, stars_ decimal) RETURNS INT AS
$$
DECLARE
  ratings_ INT;
  rating_ decimal;
  item_id_ INT;
  returned_rating INT;
BEGIN
  -- find the previous item's ratings
  SELECT items.ratings, items.rating, items.item_id INTO ratings_, rating_, item_id_
    FROM order_contents
         JOIN item_revisions
                ON order_contents.item_revision_id = item_revisions.item_revision_id
         JOIN items
                ON item_revisions.item_id = items.item_id
    WHERE order_contents.order_content_id = order_content_id_;
  -- update the accumulated rating
  UPDATE items SET ratings = ratings_ + 1, rating = ((ratings_ * rating_) + stars_) / (ratings_ + 1)
    WHERE items.item_id = item_id_;
  -- insert the review
  INSERT INTO reviews (order_content_id, title, description, stars)
    VALUES (order_content_id_, title_, description_, stars_)
           RETURNING review_id INTO returned_rating;
  RETURN returned_rating;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;
