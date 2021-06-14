# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda"; # or "nodev" for efi only
  # efiSupport = true;
  # efiInstallAsRemovable = true;
  };

  networking = {
    hostName = "t430-pc"; # Define your hostname.
    networkmanager.enable = true;  # Enables wireless support via nm
    useDHCP = false;
    interfaces.enp0s25.useDHCP = true;
    interfaces.wlp3s0.useDHCP = true;
  };

  # Set your time zone.
  time.timeZone = "America/Chihuahua";

  # Select internationalisation properties.
  i18n.defaultLocale = "es_MX.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "la-latin1";
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    # desktopManager.xfce.enable = true;
#    desktopManager.plasma5.enable = true;
    displayManager.lightdm.enable = true; # enable or disable lightdm
#    displayManager.sddm.enable = true;
    #displayManager.startx.enable = true; # if lightdm is enabled, then startx should be disabled
    windowManager.herbstluftwm.enable = true; # install herbstluftwm
    layout = "latam"; # layout of keyboard
    libinput.enable = true; #enable touchpad
  };

  services.xserver.serverFlagsSection =
    ''
    Option "StandbyTime" "30" 
    Option "BlankTime" "30"
    Option "SuspendTime" "40"
    Option "OffTime""30"
    '';
  
  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.zsh = {
    enable = true; #add zsh as the shell
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
  };

   users.users.juan = {
     isNormalUser = true;
     home = "/home/juan";
     shell = pkgs.zsh;
     description = "Juan Adrián Castro Quintana";
     extraGroups = [ "wheel" "networkmanager" "users" ]; # Enable ‘sudo’ for the user.
   };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
   environment.systemPackages = with pkgs; [
     vim
     neovim
     wget
     firefox
     emacs
     git
     stow
     fish
     pavucontrol
     ffmpeg
   ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

