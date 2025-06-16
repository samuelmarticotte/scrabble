#!/usr/bin/env bash
# Setup script for the Scrabble Erlang application on Ubuntu 24.04
set -e

# Update package index
sudo apt-get update

# Install Erlang/OTP and rebar3 build tool
sudo apt-get install -y erlang rebar3

# Compile the project
rebar3 compile

# Completion message
echo "Setup complete. You can run the game with 'rebar3 shell' and then 'play:game().'"
