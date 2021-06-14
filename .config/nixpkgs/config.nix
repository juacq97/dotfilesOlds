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
				picom
				redshift
				(polybar.override { pulseSupport = true; })
        lxappearance
        lxsession
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
				playerctl
				youtube-dl
				];
			};

		misc = pkgs.buildEnv {
			name = "misc";
			paths = [
				autorandr
				flameshot
				rofi
				alacritty
				kdeconnect
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
        pamixer
        #unzip

				#zathura-pdf-mupdf
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



				
				

