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
  created_on TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
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
  FOREIGN KEY (email) REFERENCES users (email)
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
  expiration TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP + INTERVAL '1 day',
  FOREIGN KEY (user_id) REFERENCES users (user_id)
);

/*
A chef is a Local Cooking user that we vet to have the power to create their own menus.
*/
CREATE TABLE IF NOT EXISTS chefs (
  user_id INT NOT NULL,
  chef_id serial PRIMARY KEY,
  public_name VARCHAR ( 255 ) NOT NULL,
  profile VARCHAR ( 1024 ),
  enrolled_on TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users (user_id)
);

/*
At Local Cooking, we're going to grow our chef's credentials over time. Each new trend
will require new skills and certifications - for instance, being trained in gluten intolerance,
or peanut allergies, could be a specific trade. Furthermore, we may certify someone as a
vegan cook, which may grant them special interest. Likewise, in the future, chefs may issue
certifications of their own, which they use to vet other chefs and grow their academia.
*/
CREATE TABLE IF NOT EXISTS credentials (
  credential_id serial PRIMARY KEY,
  title VARCHAR ( 255 ) NOT NULL,
  description VARCHAR ( 1024 ) NOT NULL,
);

/*
Mapping between chefs and their credentials.
*/
CREATE TABLE IF NOT EXISTS chef_credentials (
  chef_id INT NOT NULL,
  credential_id INT NOT NULL,
  FOREIGN KEY (chef_id) REFERENCES chefs (chef_id),
  FOREIGN KEY (credential_id) REFERENCES credentials (credential_id)
);
CREATE UNIQUE INDEX idx_chef_credentials
  ON chef_credentials (chef_id, credential_id); /*A chef can't have two of the same credential*/

CREATE TABLE IF NOT EXISTS menus (
  menu_id serial PRIMARY KEY,
);

CREATE TABLE IF NOT EXISTS item (
  item_id serial PRIMARY KEY,
  menu_id INT NOT NULL,
  title VARCHAR ( 255 ) NOT NULL,
  description VARCHAR ( ),
  FOREIGN KEY (menu_id) REFERENCES menus (menu_id)
);
