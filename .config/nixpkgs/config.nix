with (import <nixpkgs> {});
{
	allowUnfree = true;
	joypixels.acceptLicense = true;
	packageOverrides = pkgs: with pkgs; {
    
    
		themes = pkgs.buildEnv {
			name = "themes";
			paths = [
        flat-remix-icon-theme
        flat-remix-gtk
			];
		};
    
		plasma-pkgs = pkgs.buildEnv {
			name = "plasma-pkgs";
			paths = [
        plasma-pass
        plasma-applet-virtual-desktop-bar
        qt5.qtwebsockets
        latte-dock
        plasma5Packages.applet-window-buttons
        krunner-pass
        krunner-symbols
        kcolorchooser
        ark
			];
		};

	 gnome-ext = pkgs.buildEnv {
			name = "gnome-ext";
 			paths = [
        gnome.gnome-tweak-tool
	gnome-menus
	];
};
    
		wm = pkgs.buildEnv {
			name = "wm";
			paths = [
				dunst
				sxhkd
        #				xfce.xfce4_power_manager_gtk3
				nitrogen
				clipmenu
        # Use the ibhagwan fork of picom, with blur and rounded corners
        picom
				redshift
				(polybar.override { pulseSupport = true; })
        lxappearance-gtk2
        lxsession
        libsForQt5.qtstyleplugins
			];
		};
    
		media = pkgs.buildEnv {
			name = "media";
			paths = [
				mpv
				sxiv
				pqiv
				gimp
				krita
				kdenlive
				ffmpeg
#				playerctl
				youtube-dl
			];
		};
    
		misc = pkgs.buildEnv {
			name = "misc";
			paths = [
				kdeconnect
				autorandr
				flameshot
				rofi
				(firefox.override {extraNativeMessagingHosts = [ 
					passff-host
					]; })
				pavucontrol
				alacritty
				libreoffice-still
				xfce.thunar
				zathura
				killall
				htop
				lm_sensors
        pass-otp
        passff-host
        pfetch
        libnotify
        wmctrl
        xclip
        poppler_utils
        playerctl
        pamixer
        unzip
        p7zip
        brightnessctl
        nextcloud-client
        binutils
			];
		};
    
		fonts = pkgs.buildEnv {
			name = "fonts";
			paths = [
				liberation_ttf
				source-code-pro
				joypixels
				opensans-ttf
				material-design-icons
        fira-code
        fira
        comfortaa
			];
		};
    
    latex = pkgs.buildEnv {
      name = "latex";
        paths = [
               (texlive.combine {
                 inherit (texlive)
                   scheme-medium
                   capt-of
                   wrapfig
                   collectbox
                   beamerposter
                   type1cm
                   textpos
                   adjustbox;
               })
             ];
    };
    
    # Dev libraries. Though I should use nix-shell for libraries, I need some like jq
		dev = pkgs.buildEnv {
			name = "dev";
			paths = [
        jq
			];
		};
    
	};
}
  
  
  
	
	
  
