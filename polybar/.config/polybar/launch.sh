#!/bin/bash

killall -q polybar

while pgrep -x polybar > /dev/null; do sleep 1; done

polybar ws &
polybar time &
polybar vitals &
polybar tray &

xdo lower -n polybar
wait

echo "Bar launched..."
