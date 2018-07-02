#!/bin/bash

ps ax | awk '$5 ~ /^ssh-agent/{print "kill "$1}' | sh
ssh-agent > ~/.ssh-agent.out
