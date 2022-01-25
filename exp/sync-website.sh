#! /bin/bash

rsync -e "ssh -i ~/doc/env/ssh/id_rsa" -av --progress --stats --delete --force ~/dev/html/ceba-vis sol-nhl@sgsdm.isk.lu.se:/tmp/
ssh -i ~/doc/env/ssh/id_rsa -t sol-nhl@sgsdm.isk.lu.se "sudo bash /tmp/ceba-vis/src/remote-commands.sh"

