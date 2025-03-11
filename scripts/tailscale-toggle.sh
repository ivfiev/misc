#!/bin/bash

if systemctl is-active --quiet tailscaled; then
    echo "🛑 Tailscale is running. Stopping it now..."
    sudo tailscale down
    sudo systemctl stop tailscaled
    sudo systemctl disable --now tailscaled
    echo "✅ Tailscale stopped (it will stay off after reboot)."
else
    echo "🚀 Starting Tailscale (temporarily, no DNS hijacking)..."
    sudo systemctl start tailscaled
    sudo tailscale up --accept-dns=false
    echo "✅ Tailscale started without DNS hijacking."
    tailscale status
fi
