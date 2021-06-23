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
DECLARE
  found_user_ INT;
  returned_session_ uuid;
BEGIN
  SELECT user_id INTO found_user_ FROM users WHERE email = email_ AND deactivated_on IS NULL;
  -- Find an active user with the same email
  IF found_user_ IS NULL THEN
    INSERT INTO pending_registrations (email) VALUES (email_) RETURNING auth_token INTO returned_session_;
    RETURN returned_session_;
  ELSE
    RAISE 'User with email % already exists', email_;
  END IF;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;

CREATE OR REPLACE FUNCTION register(registration_ uuid, email_ VARCHAR, password_ BYTEA, salt_ BYTEA) RETURNS INT AS
$$
DECLARE
  uid INT;
  is_pending_ INT;
  already_exists_ INT;
BEGIN
  -- Make sure the email doesn't already exist
  SELECT 1 INTO already_exists_ FROM users
  WHERE email = email_;

  IF already_exists_ = 1 THEN
    RAISE 'email % aready registered', email_;
  ELSE
    -- Ensure a valid pending registration exists
    SELECT 1 INTO is_pending_ FROM pending_registrations
      WHERE email = email_
        AND auth_token = registration_
        AND expiration >= CURRENT_TIMESTAMP;

    IF is_pending_ = 1 THEN
      -- delete it
      DELETE FROM pending_registrations
      WHERE auth_token = registration_
      AND expiration >= CURRENT_TIMESTAMP
      AND email = email_;

      -- insert the info into the user's column
      INSERT INTO users (email, password, salt)
      VALUES (email_, password_, salt_)
      RETURNING user_id INTO uid;
      RETURN uid;
    ELSE
      RAISE 'Pending registration not found: %', registration_;
    END IF;
  END IF;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;
