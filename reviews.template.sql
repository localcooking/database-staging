/*
Reviews for a specific order's contents.

--{{ reviews }}

*/
CREATE TABLE IF NOT EXISTS api.reviews (
  review_id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
  order_content_id INT UNIQUE NOT NULL REFERENCES api.order_contents (order_content_id) ON DELETE RESTRICT, /* Only one review per order content */
  reviewed_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  title VARCHAR ( 255 ) NOT NULL,
  description VARCHAR ( 1024 ) NOT NULL,
  /* TODO images */
  stars decimal(2,1) CHECK (stars >= 1 AND stars <= 5) /* For aggregates */
);


CREATE OR REPLACE FUNCTION api.review(order_content_id_ INT, title_ VARCHAR, description_ VARCHAR, stars_ decimal) RETURNS INT AS
$$
DECLARE
  ratings_ INT;
  rating_ decimal;
  item_id_ INT;
  returned_rating INT;
BEGIN
  -- find the previous item's ratings
  SELECT items.ratings, items.rating, items.item_id INTO ratings_, rating_, item_id_
    FROM api.order_contents
         JOIN api.item_revisions
                ON order_contents.item_revision_id = item_revisions.item_revision_id
         JOIN api.items
                ON item_revisions.item_id = items.item_id
    WHERE order_contents.order_content_id = order_content_id_;
  -- update the accumulated rating
  UPDATE api.items SET ratings = ratings_ + 1, rating = ((ratings_ * rating_) + stars_) / (ratings_ + 1)
    WHERE items.item_id = item_id_;
  -- insert the review
  INSERT INTO api.reviews (order_content_id, title, description, stars)
    VALUES (order_content_id_, title_, description_, stars_)
           RETURNING review_id INTO returned_rating;
  RETURN returned_rating;
END;
$$
  LANGUAGE plpgsql
  VOLATILE
  RETURNS NULL ON NULL INPUT;
