#!/usr/bin/env bash

# NixOS Configuration Installer
# This script installs the custom NixOS configuration on any computer

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
CONFIG_REPO="https://github.com/emilio-junoy/NixOS"
TARGET_DIR="/etc/nixos"
BACKUP_DIR="/etc/nixos-backup-$(date +%Y%m%d-%H%M%S)"

# Functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if running as root
check_root() {
    if [[ $EUID -ne 0 ]]; then
        log_error "This script must be run as root (use sudo)"
        exit 1
    fi
}

# Check if NixOS
check_nixos() {
    if [[ ! -f /etc/os-release ]] || ! grep -q "NixOS" /etc/os-release; then
        log_error "This script is designed for NixOS systems only"
        exit 1
    fi
}

# Install required packages
install_dependencies() {
    log_info "Installing dependencies..."
    
    # Install git if not present
    if ! command -v git &> /dev/null; then
        nix-env -iA nixos.git
    fi
    
    # Enable flakes if not enabled
    if ! grep -q "experimental-features" /etc/nix/nix.conf 2>/dev/null; then
        log_info "Enabling Nix flakes..."
        mkdir -p /etc/nix
        echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
        systemctl restart nix-daemon
    fi
}

# Get user input
get_user_input() {
    echo
    log_info "Configuration setup - please provide the following information:"
    echo
    
    # Hostname
    read -p "Enter hostname [nixos]: " HOSTNAME
    HOSTNAME=${HOSTNAME:-nixos}
    
    # Username
    read -p "Enter username [user]: " USERNAME
    USERNAME=${USERNAME:-user}
    
    # Timezone
    echo "Available timezones (examples):"
    echo "  America/New_York, America/Los_Angeles, America/Chicago"
    echo "  Europe/London, Europe/Paris, Europe/Berlin"
    echo "  Asia/Tokyo, Asia/Shanghai, Australia/Sydney"
    read -p "Enter timezone [America/Mexico_City]: " TIMEZONE
    TIMEZONE=${TIMEZONE:-America/Mexico_City}
    
    # Locale
    echo "Available locales (examples):"
    echo "  en_US.UTF-8 (English US), en_GB.UTF-8 (English UK)"
    echo "  es_ES.UTF-8 (Spanish Spain), es_MX.UTF-8 (Spanish Mexico)"
    echo "  fr_FR.UTF-8 (French), de_DE.UTF-8 (German)"
    echo "  it_IT.UTF-8 (Italian), pt_BR.UTF-8 (Portuguese Brazil)"
    echo "  ru_RU.UTF-8 (Russian), zh_CN.UTF-8 (Chinese Simplified)"
    read -p "Enter locale [en_US.UTF-8]: " LOCALE
    LOCALE=${LOCALE:-en_US.UTF-8}
    
    # Keyboard layout
    echo "Available keyboard layouts (examples):"
    echo "  us (US English), uk (UK English), es (Spanish) latam (Spanish latam)"
    echo "  fr (French), de (German), it (Italian)"
    echo "  ru (Russian), br (Brazilian), dvorak (Dvorak)"
    read -p "Enter keyboard layout [us]: " KB_LAYOUT
    KB_LAYOUT=${KB_LAYOUT:-us}
    
    # Keyboard variant (optional)
    echo "Keyboard variants for '$KB_LAYOUT' (examples - leave empty for default):"
    case $KB_LAYOUT in
        "us")
            echo "  intl (US International), dvorak (Dvorak), colemak (Colemak)"
            ;;
        "es")
            echo "  nodeadkeys (No dead keys), cat (Catalan)"
            ;;
        "fr")
            echo "  nodeadkeys (No dead keys), bepo (Bépo), oss (Alternative)"
            ;;
        "de")
            echo "  nodeadkeys (No dead keys), neo (Neo layout)"
            ;;
        *)
            echo "  (Common: nodeadkeys, intl, dvorak - check your layout documentation)"
            ;;
    esac
    read -p "Enter keyboard variant []: " KB_VARIANT
    
    # Git configuration
    read -p "Enter Git user name: " GIT_NAME
    read -p "Enter Git user email: " GIT_EMAIL
    
    # Hardware detection
    echo
    log_info "Detecting hardware..."
    
    # GPU detection
    if lspci | grep -i nvidia &> /dev/null; then
        GPU_TYPE="nvidia"
        log_info "NVIDIA GPU detected"
    elif lspci | grep -i amd | grep -i vga &> /dev/null; then
        GPU_TYPE="amd"
        log_info "AMD GPU detected"
    else
        GPU_TYPE="intel"
        log_info "Using Intel/integrated graphics"
    fi
    
    # CPU detection
    if grep -i "amd" /proc/cpuinfo &> /dev/null; then
        CPU_TYPE="amd"
        log_info "AMD CPU detected"
    else
        CPU_TYPE="intel"
        log_info "Intel CPU detected"
    fi
    
    echo
    log_info "Configuration summary:"
    echo "  Hostname: $HOSTNAME"
    echo "  Username: $USERNAME"
    echo "  Timezone: $TIMEZONE"
    echo "  Locale: $LOCALE"
    echo "  Keyboard Layout: $KB_LAYOUT${KB_VARIANT:+,$KB_VARIANT}"
    echo "  Git Name: $GIT_NAME"
    echo "  Git Email: $GIT_EMAIL"
    echo "  GPU: $GPU_TYPE"
    echo "  CPU: $CPU_TYPE"
    echo
    
    read -p "Continue with installation? [y/N]: " CONFIRM
    if [[ ! $CONFIRM =~ ^[Yy]$ ]]; then
        log_info "Installation cancelled"
        exit 0
    fi
}

# Backup existing configuration
backup_existing() {
    if [[ -d $TARGET_DIR ]]; then
        log_info "Backing up existing configuration to $BACKUP_DIR"
        cp -r $TARGET_DIR $BACKUP_DIR
        log_success "Backup created at $BACKUP_DIR"
    fi
}

# Clone configuration
clone_config() {
    log_info "Cloning configuration from $CONFIG_REPO"
    
    # Remove existing directory if it exists
    if [[ -d $TARGET_DIR ]]; then
        rm -rf $TARGET_DIR
    fi
    
    # Clone the repository
    git clone $CONFIG_REPO $TARGET_DIR
    
    # Initialize git if not already done
    cd $TARGET_DIR
    if [[ ! -d .git ]]; then
        git init
        git add .
        git commit -m "Initial configuration"
    fi
    
    log_success "Configuration cloned successfully"
}

# Generate hardware configuration
generate_hardware_config() {
    log_info "Generating hardware configuration..."
    nixos-generate-config --show-hardware-config > $TARGET_DIR/hardware-configuration.nix
    log_success "Hardware configuration generated"
}

# Update configuration files
update_config() {
    log_info "Updating configuration files..."
    
    cd $TARGET_DIR
    
    # Update flake.nix with new hostname
    sed -i "s/HOSTNAME = \"mou\"/HOSTNAME = \"$HOSTNAME\"/g" flake.nix
    
    # Update configuration.nix
    # Replace hostname
    sed -i "s/hostname = \"mou\"/hostname = \"$HOSTNAME\"/g" configuration.nix
    
    # Replace timezone
    sed -i "s|timezone = \"America/Mexico_City\"|timezone = \"$TIMEZONE\"|g" configuration.nix
    
    # Replace locale
    sed -i "s/locale = \"en_US.UTF-8\"/locale = \"$LOCALE\"/g" configuration.nix
    
    # Replace username in configuration.nix
    sed -i "s/users\.\"mou\"/users.\"$USERNAME\"/g" configuration.nix
    sed -i "s/users\.users\.mou/users.users.$USERNAME/g" configuration.nix
    sed -i "s/mou\.services\.custom\.nordvpn/$USERNAME.services.custom.nordvpn/g" configuration.nix
    
    # Update GPU configuration
    if [[ $GPU_TYPE == "nvidia" ]]; then
        sed -i 's|# inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-nvidia|inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-nvidia|g' configuration.nix
        sed -i 's|inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-amd|# inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-amd|g' configuration.nix
    elif [[ $GPU_TYPE == "amd" ]]; then
        sed -i 's|# inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-amd|inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-amd|g' configuration.nix
        sed -i 's|inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-nvidia|# inputs.hydenix.inputs.nixos-hardware.nixosModules.common-gpu-nvidia|g' configuration.nix
    fi
    
    # Update CPU configuration
    if [[ $CPU_TYPE == "intel" ]]; then
        sed -i 's|inputs.hydenix.inputs.nixos-hardware.nixosModules.common-cpu-amd|# inputs.hydenix.inputs.nixos-hardware.nixosModules.common-cpu-amd|g' configuration.nix
        sed -i 's|# inputs.hydenix.inputs.nixos-hardware.nixosModules.common-cpu-intel|inputs.hydenix.inputs.nixos-hardware.nixosModules.common-cpu-intel|g' configuration.nix
    fi
    
    # Update Git configuration in home-manager
    if [[ -n "$GIT_NAME" && -n "$GIT_EMAIL" ]]; then
        sed -i "s/name = \"emilio-junoy\"/name = \"$GIT_NAME\"/g" modules/hm/default.nix
        sed -i "s/email = \"emilio.junoy@gmail.com\"/email = \"$GIT_EMAIL\"/g" modules/hm/default.nix
    fi
    
    log_success "Configuration files updated"
}

# Build and switch to new configuration
build_system() {
    log_info "Building and switching to new configuration..."
    log_warning "This may take a while on first build..."
    
    cd $TARGET_DIR
    
    # Stage files for git (required for flakes)
    git add .
    
    # Build the system
    if nixos-rebuild switch --flake . --show-trace; then
        log_success "System built and activated successfully!"
    else
        log_error "Failed to build system. Check the output above for errors."
        log_info "You can try to fix the issues and run: nixos-rebuild switch --flake ."
        exit 1
    fi
}

# Post-installation tasks
post_install() {
    log_info "Performing post-installation tasks..."
    
    # Create user if it doesn't exist
    if ! id "$USERNAME" &>/dev/null; then
        log_info "Creating user $USERNAME..."
        useradd -m -G wheel,networkmanager,video,wireshark,nordvpn -s /run/current-system/sw/bin/zsh "$USERNAME"
        echo "$USERNAME:hydenix" | chpasswd
        log_warning "Default password set to 'hydenix' - please change it after reboot!"
    fi
    
    # Set ownership of configuration directory
    chown -R root:root $TARGET_DIR
    
    log_success "Post-installation tasks completed"
}

# Display final instructions
final_instructions() {
    echo
    log_success "Installation completed successfully!"
    echo
    log_info "Next steps:"
    echo "  1. Reboot your system: sudo reboot"
    echo "  2. Log in as '$USERNAME' with password 'hydenix'"
    echo "  3. Change your password: passwd"
    echo "  4. Run 'hyde-shell reload' to generate theme cache"
    echo
    log_info "Configuration location: $TARGET_DIR"
    log_info "Backup location: $BACKUP_DIR (if created)"
    echo
    log_info "To update the system in the future:"
    echo "  cd $TARGET_DIR"
    echo "  nix flake update"
    echo "  sudo nixos-rebuild switch --flake ."
    echo
    log_warning "Remember to commit your changes to git after customization!"
}

# Main installation flow
main() {
    echo "====================================="
    echo "    NixOS Configuration Installer    "
    echo "====================================="
    echo
    
    check_root
    check_nixos
    install_dependencies
    get_user_input
    backup_existing
    clone_config
    generate_hardware_config
    update_config
    build_system
    post_install
    final_instructions
}

# Handle interruption gracefully
trap 'log_error "Installation interrupted!"; exit 1' INT TERM

# Run main function
main "$@"

