#! /bin/bash

sudo rm -Rf /var/www/sgsdm.isk.lu.se/ceba
sudo cp -R /tmp/ceba-vis/ /var/www/sgsdm.isk.lu.se/ceba
# if needed, get git repo
#rm -Rf ~/lib/js/*; mkdir -p ~/lib/js/jspsych; git clone https://github.com/jspsych/jsPsych.git ~/lib/js/jspsych/
# change owner, also affecting jspsych lib ?
sudo chown -R www-data:www-data /var/www/sgsdm.isk.lu.se/ceba

