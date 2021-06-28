#! /bin/bash

# Populate index

ltext "index.template.sql users.template.sql pendingRegistrations.template.sql sessions.template.sql chefs.template.sql menus.template.sql items.template.sql carts.template.sql orders.template.sql reviews.template.sql" \
      -r "users.template.sql" \
      -r "pendingRegistrations.template.sql" \
      -r "sessions.template.sql" \
      -r "chefs.template.sql" \
      -r "menus.template.sql" \
      -r "items.template.sql" \
      -r "carts.template.sql" \
      -r "orders.template.sql" \
      -r "reviews.template.sql" \
      > stage.template.sql

# Populate documentation
sudo -i -u postgres createdb localcooking_tmp

mkdir -p tmp/

export WD=`pwd`

sudo -i -u postgres psql -d localcooking_tmp -f $WD/stage.template.sql
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.users" > tmp/users
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.pending_registrations" > tmp/pendingRegistrations
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.active_pending_registrations" > tmp/activePendingRegistrations
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.sessions" > tmp/sessions
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.active_sessions" > tmp/activeSessions
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.chefs" > tmp/chefs
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.credentials" > tmp/credentials
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.chef_credentials" > tmp/chefCredentials
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.menus" > tmp/menus
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.items" > tmp/items
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.item_revisions" > tmp/itemRevisions
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.latest_active_item_revisions" > tmp/latestActiveItemRevisions
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.menu_item_mapping" > tmp/menuItemMapping
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.carts" > tmp/carts
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.orders" > tmp/orders
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.order_contents" > tmp/orderContents
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ api.reviews" > tmp/reviews

ltext "stage.template.sql tmp/users tmp/pendingRegistrations tmp/activePendingRegistrations tmp/sessions tmp/activeSessions tmp/chefs tmp/credentials tmp/chefCredentials tmp/menus tmp/items tmp/itemRevisions tmp/latestActiveItemRevisions tmp/menuItemMapping tmp/carts tmp/orders tmp/orderContents tmp/reviews" \
      -r "tmp/users" \
      -r "tmp/pendingRegistrations" \
      -r "tmp/activePendingRegistrations" \
      -r "tmp/sessions" \
      -r "tmp/activeSessions" \
      -r "tmp/chefs" \
      -r "tmp/credentials" \
      -r "tmp/chefCredentials" \
      -r "tmp/menus" \
      -r "tmp/items" \
      -r "tmp/itemRevisions" \
      -r "tmp/latestActiveItemRevisions" \
      -r "tmp/menuItemMapping" \
      -r "tmp/carts" \
      -r "tmp/orders" \
      -r "tmp/orderContents" \
      -r "tmp/reviews" \
      > stage.sql

rm -r tmp/

sudo -i -u postgres dropdb localcooking_tmp
