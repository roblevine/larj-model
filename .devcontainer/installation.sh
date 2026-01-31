#!/bin/sh
echo "* starting installation script"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "** update and install apt packages"
sudo apt update
sudo apt upgrade -y
sudo apt install -y vim dos2unix tmux curl

# Create symlink for libcrypt on ARM machines
if [ "$(uname -m)" = "aarch64" ]; then
    echo "** setting up libcrypt symlink for ARM64"
    sudo mkdir -p /usr/lib/aarch64-linux-gnu
    sudo ln -sf /usr/lib/aarch64-linux-gnu/libcrypt.so.1 /usr/lib/aarch64-linux-gnu/libcrypt-c6b9afc0.so.1
fi

echo "** install Claude Code"
curl -fsSL https://claude.ai/install.sh | bash

if [ "${ENABLE_SSH_SERVER_IN_DEVCONTAINER:-false}" = "true" ]; then
    echo "** enabling SSH server in devcontainer"
    sh "$SCRIPT_DIR/install_ssh_server.sh"
fi

echo "* installation script complete"