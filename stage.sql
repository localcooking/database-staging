/*
  localcooking_20210525=# SELECT * FROM users;
   user_id | email | password | created_on | last_login
  ---------+-------+----------+------------+------------
  (0 rows)
 */

CREATE TABLE IF NOT EXISTS users (
       user_id serial PRIMARY KEY,
       email VARCHAR ( 255 ) UNIQUE NOT NULL,
       password VARCHAR ( 255 ) NOT NULL,
       created_on TIMESTAMP NOT NULL,
       last_login TIMESTAMP
);

/*
Here, during registration, we email the address a link with a unique token,
to verify the email is authentic. Furthermore, in the email with a "click here to complete"
link, we'll also include everything they put in the registration form as url parameters
(pkdf2 encrypting the password first).

  localcooking_20210525=# SELECT * FROM pending_registrations;
   email | auth_token
  -------+------------
  (0 rows)
*/
CREATE TABLE IF NOT EXISTS pending_registrations (
       email VARCHAR ( 255 ) NOT NULL,
       auth_token VARCHAR ( 255 ) PRIMARY KEY,
       FOREIGN KEY (email)
               REFERENCES users (email)
);

/*
Every time a user successfully logs in, we'll issue them a session token that gets kept in a
cookie, and verified every time they check out a page on the website. Every time communication
is valid in a good session, the expiration resets to now + 1 day. Then as a cron job, the
database will prune old data during off hours.

  localcooking_20210525=# SELECT * FROM sessions;
   user_id | session_token | expiration
  ---------+---------------+------------
  (0 rows)
*/
CREATE TABLE IF NOT EXISTS sessions (
       user_id INT NOT NULL,
       session_token VARCHAR ( 255 ) PRIMARY KEY,
       expiration TIMESTAMP NOT NULL,
       FOREIGN KEY (user_id)
               REFERENCES users (user_id)
);
