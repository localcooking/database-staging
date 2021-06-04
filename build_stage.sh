#! /bin/bash

sudo -i -u postgres createdb localcooking_tmp

mkdir -p tmp/

export WD=`pwd`

sudo -i -u postgres psql -d localcooking_tmp -f $WD/stage.template.sql
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ users" > tmp/users
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ pending_registrations" > tmp/pendingRegistrations
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ active_pending_registrations" > tmp/activePendingRegistrations
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ sessions" > tmp/sessions
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ active_sessions" > tmp/activeSessions
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ chefs" > tmp/chefs
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ credentials" > tmp/credentials
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ chef_credentials" > tmp/chefCredentials
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ menus" > tmp/menus
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ items" > tmp/items
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ item_revisions" > tmp/itemRevisions
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ menu_item_mapping" > tmp/menuItemMapping
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ carts" > tmp/carts
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ orders" > tmp/orders
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ order_contents" > tmp/orderContents
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ reviews" > tmp/reviews

ltext "stage.template.sql tmp/users tmp/pendingRegistrations tmp/activePendingRegistrations tmp/sessions tmp/activeSessions tmp/chefs tmp/credentials tmp/chefCredentials tmp/menus tmp/items tmp/itemRevisions tmp/menuItemMapping tmp/carts tmp/orders tmp/orderContents tmp/reviews" \
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
      -r "tmp/menuItemMapping" \
      -r "tmp/carts" \
      -r "tmp/orders" \
      -r "tmp/orderContents" \
      -r "tmp/reviews" \
      > stage.sql

rm -r tmp/

sudo -i -u postgres dropdb localcooking_tmp
