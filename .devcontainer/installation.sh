#!/bin/sh
echo "* starting installation script"

echo "** update and install apt packages"
sudo apt update
sudo apt upgrade -y
sudo apt install -y vim dos2unix tmux

# Create symlink for libcrypt on ARM machines
if [ "$(uname -m)" = "aarch64" ]; then
    echo "** setting up libcrypt symlink for ARM64"
    sudo mkdir -p /usr/lib/aarch64-linux-gnu
    sudo ln -sf /usr/lib/aarch64-linux-gnu/libcrypt.so.1 /usr/lib/aarch64-linux-gnu/libcrypt-c6b9afc0.so.1
fi

echo "** install uv"
pipx install uv

echo "** install Claude Code"
npm install -g @anthropic-ai/claude-code

echo "* installation script complete"