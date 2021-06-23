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

-- FIXME raise when session doesn't exist?
CREATE OR REPLACE FUNCTION get_logged_in_user_id(session_token_ uuid) RETURNS INT AS
$$
  SELECT user_id FROM sessions WHERE session_token = session_token_;
$$
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT;

CREATE OR REPLACE FUNCTION login(email_ VARCHAR, password_ BYTEA) RETURNS uuid AS
$$
DECLARE
  -- user_id_ INT;
  -- deactivated_on_ timestamptz;
  correct_creds RECORD;
  active_session uuid;
BEGIN
  SELECT INTO correct_creds user_id, deactivated_on FROM users
    WHERE email = email_ AND password = password_;
  IF correct_creds.deactivated_on IS NOT NULL THEN
    RAISE 'User has been deactivated on %', correct_creds.deactivated_on;
  ELSE
    UPDATE users SET last_login = CURRENT_TIMESTAMP, last_active = CURRENT_TIMESTAMP
      WHERE user_id = correct_creds.user_id; -- update last login
    INSERT INTO sessions (user_id) VALUES (correct_creds.user_id)
      RETURNING session_token INTO active_session; -- insert session
    RETURN active_session;
  END IF;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;

-- Should be done last in any authenticated transaction
CREATE OR REPLACE FUNCTION touch_session(session_token_ uuid) RETURNS uuid AS
$$
DECLARE
  user_id_ INT;
  active_session uuid;
BEGIN
  SELECT get_logged_in_user_id(session_token_) INTO user_id_; -- find logged-in user
  UPDATE users SET last_active = CURRENT_TIMESTAMP WHERE user_id = user_id_; -- update last active
  UPDATE sessions SET session_token = gen_random_uuid()
    WHERE session_token = session_token_ AND user_id = user_id_
    RETURNING session_token INTO active_session;
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
