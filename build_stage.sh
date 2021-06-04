#! /bin/bash

sudo -i -u postgres psql -c "DROP DATABASE localcooking_tmp; CREATE DATABASE localcooking_tmp;"

mkdir -p tmp/

sudo -i -u postgres psql -d localcooking_tmp -f /home/athan/dev/localcooking/database/staging/stage.template.sql
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ users" > tmp/users
sudo -i -u postgres psql -d localcooking_tmp -c "\\d+ pending_registrations" > tmp/pendingRegistrations

ltext "stage.template.sql tmp/users tmp/pendingRegistrations" -r "tmp/users" -r "tmp/pendingRegistrations" > stage.sql
