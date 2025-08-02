#!/usr/bin/env bash

# NixOS Configuration Script for Hydenix
# Description: Applies Hydenix configuration on existing NixOS system

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration variables
HOSTNAME="mou"
USERNAME="mou"
CONFIG_REPO_PATH="/home/$USERNAME/NixOS"

# Output helpers
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_step() {
    echo -e "${BLUE}[STEP]${NC} $1"
}

# Check for root privileges
check_root() {
    if [[ $EUID -ne 0 ]]; then
        print_error "This script must be run as root (use sudo)"
        exit 1
    fi
}

# Ensure NixOS is already installed
check_nixos_installed() {
    if [[ -f /etc/NIXOS ]]; then
        print_status "NixOS installation detected. Proceeding with configuration."
    else
        print_error "No existing NixOS installation detected. Please install NixOS first."
        exit 1
    fi
}

# Generate hardware-configuration.nix
generate_hardware_config() {
    print_step "Generating hardware configuration..."

    nixos-generate-config --dir "$CONFIG_REPO_PATH"

    print_status "Hardware configuration ready"
}

# Main logic
main() {
    print_status "Starting configuration on existing NixOS system with Hydenix configuration"
    print_status "Configuration: Custom Hydenix setup for user '$USERNAME'"
    print_status "Hostname: $HOSTNAME"
    echo

    check_root
    check_nixos_installed
    generate_hardware_config

    print_step "Applying NixOS configuration"
    nixos-rebuild switch --flake .

    echo
    print_status "=== Configuration Applied! ==="
    print_status "NixOS with Hydenix configurations applied successfully."
    echo
    print_status "Post-configuration notes:"
    echo "  - To rebuild the system: sudo nixos-rebuild switch --flake ."
    echo "  - Configuration location: /home/$USERNAME/NixOS"
    print_warning "Remember to:"
    echo "  1. Update your Git configuration in modules/hm/default.nix"
    echo "  2. Customize your hardware-configuration.nix if needed"
    echo
    print_status "Configuration complete. No need to reboot unless you made major changes."
}

# Execute if run directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi

