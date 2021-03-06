/*
The cart for people to file orders

--{{ carts }}

*/
CREATE TABLE IF NOT EXISTS api.carts (
  cart_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  user_id INT NOT NULL REFERENCES api.users (user_id) ON DELETE RESTRICT,
  item_id INT NOT NULL REFERENCES api.items (item_id) ON DELETE RESTRICT, -- menu_item_mapping is used only for views
  unseen_revision INT REFERENCES api.item_revisions (item_revision_id) ON DELETE RESTRICT, -- when creating a new revision, also set all carts to that revision as well.
  quantity INT NOT NULL DEFAULT 1 CHECK (quantity > 0),
  added_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (user_id, item_id) /* increment quantity instead - FIXME can I just update on insert / delete? */
);
-- When a user checks out, their cart rows are deleted - nothing references this table.

CREATE OR REPLACE FUNCTION api.get_cart(user_id_ INT) RETURNS record AS
$$
  SELECT carts.cart_id, carts.item_id, carts.unseen_revision, carts.quantity, carts.added_on
  AS cart_id, item_id, unseen_revision, quantity, added_on
  FROM api.carts
  WHERE user_id = user_id_
  ORDER BY added_on DESC; -- TODO pagination?
$$
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT;
