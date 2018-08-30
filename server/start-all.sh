#!/usr/bin/env bash
# startup script

which adb

/bin/droidstar-demo-server --init-delay 120 --emu-address 127.0.0.1 &

/start.sh
