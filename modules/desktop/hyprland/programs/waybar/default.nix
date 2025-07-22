{pkgs, ...}: {
  fonts.packages = with pkgs.nerd-fonts; [jetbrains-mono];
  home-manager.sharedModules = [
    (_: {
      programs.waybar = {
        enable = true;
        systemd = {
          enable = false;
          target = "graphical-session.target";
        };
        settings = [
          {
            layer = "top";
            position = "top";
            mode = "dock"; # Fixes fullscreen issues
            height = 32; # 35
            exclusive = true;
            passthrough = false;
            gtk-layer-shell = true;
            ipc = true;
            fixed-center = true;
            margin-top = 10;
            margin-left = 10;
            margin-right = 10;
            margin-bottom = 0;

            modules-left = ["hyprland/workspaces" "cava"];
            # modules-center = ["clock" "custom/notification"];
            modules-center = ["idle_inhibitor" "clock"];
            modules-right = ["custom/gpuinfo" "cpu" "memory" "backlight" "pulseaudio" "bluetooth" "network" "tray" "battery"];

            "custom/notification" = {
              tooltip = false;
              format = "{icon}";
              format-icons = {
                notification = "<span foreground='red'><sup></sup></span>";
                none = "";
                dnd-notification = "<span foreground='red'><sup></sup></span>";
                dnd-none = "";
                inhibited-notification = "<span foreground='red'><sup></sup></span>";
                inhibited-none = "";
                dnd-inhibited-notification = "<span foreground='red'><sup></sup></span>";
                dnd-inhibited-none = "";
              };
              return-type = "json";
              exec-if = "which swaync-client";
              exec = "swaync-client -swb";
              on-click = "swaync-client -t -sw";
              on-click-right = "swaync-client -d -sw";
              escape = true;
            };

            "custom/colour-temperature" = {
              format = "{} ";
              exec = "wl-gammarelay-rs watch {t}";
              on-scroll-up = "busctl --user -- call rs.wl-gammarelay / rs.wl.gammarelay UpdateTemperature n +100";
              on-scroll-down = "busctl --user -- call rs.wl-gammarelay / rs.wl.gammarelay UpdateTemperature n -100";
            };
            "custom/cava_mviz" = {
              exec = "${../../scripts/WaybarCava.sh}";
              format = "{}";
            };
            "cava" = {
              hide_on_silence = false;
              framerate = 60;
              bars = 10;
              format-icons = ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"];
              input_delay = 1;
              # "noise_reduction" = 0.77;
              sleep_timer = 5;
              bar_delimiter = 0;
              on-click = "playerctl play-pause";
            };
            "custom/gpuinfo" = {
              exec = "${../../scripts/gpuinfo.sh}";
              return-type = "json";
              format = " {}";
              interval = 5; # once every 5 seconds
              tooltip = true;
              max-length = 1000;
            };
            "custom/icon" = {
              # format = " ";
              exec = "echo ' '";
              format = "{}";
            };
            "mpris" = {
              format = "{player_icon} {title} - {artist}";
              format-paused = "{status_icon} <i>{title} - {artist}</i>";
              player-icons = {
                default = "▶";
                spotify = "";
                mpv = "󰐹";
                vlc = "󰕼";
                firefox = "";
                chromium = "";
                kdeconnect = "";
                mopidy = "";
              };
              status-icons = {
                paused = "⏸";
                playing = "";
              };
              ignored-players = ["firefox" "chromium"];
              max-length = 30;
            };
            "temperature" = {
              hwmon-path = "/sys/class/hwmon/hwmon1/temp1_input";
              critical-threshold = 83;
              format = "{icon} {temperatureC}°C";
              format-icons = ["" "" ""];
              interval = 10;
            };
            "hyprland/language" = {
              format = "{short}"; # can use {short} and {variant}
              on-click = "${../../scripts/keyboardswitch.sh}";
            };
            "hyprland/workspaces" = {
              disable-scroll = true;
              all-outputs = true;
              active-only = false;
              on-click = "activate";
              persistent-workspaces = {
                "*" = [1 2 3 4 5 6 7 8 9 10];
              };
            };

            "hyprland/window" = {
              format = "  {}";
              separate-outputs = true;
              rewrite = {
                "harvey@hyprland =(.*)" = "$1 ";
                "(.*) — Mozilla Firefox" = "$1 󰈹";
                "(.*)Mozilla Firefox" = " Firefox 󰈹";
                "(.*) - Visual Studio Code" = "$1 󰨞";
                "(.*)Visual Studio Code" = "Code 󰨞";
                "(.*) — Dolphin" = "$1 󰉋";
                "(.*)Spotify" = "Spotify 󰓇";
                "(.*)Spotify Premium" = "Spotify 󰓇";
                "(.*)Steam" = "Steam 󰓓";
              };
              max-length = 1000;
            };

            "idle_inhibitor" = {
              format = "{icon}";
              format-icons = {
                activated = "󰥔";
                deactivated = "";
              };
            };

            "clock" = {
              format = "{:%a %d %b %R}";
              # format = "{:%R 󰃭 %d·%m·%y}";
              format-alt = "{:%I:%M %p}";
              tooltip-format = "<tt>{calendar}</tt>";
              calendar = {
                mode = "month";
                mode-mon-col = 3;
                on-scroll = 1;
                on-click-right = "mode";
                format = {
                  months = "<span color='#ffead3'><b>{}</b></span>";
                  weekdays = "<span color='#ffcc66'><b>{}</b></span>";
                  today = "<span color='#ff6699'><b>{}</b></span>";
                };
              };
              actions = {
                on-click-right = "mode";
                on-click-forward = "tz_up";
                on-click-backward = "tz_down";
                on-scroll-up = "shift_up";
                on-scroll-down = "shift_down";
              };
            };

            "cpu" = {
              interval = 10;
              format = "󰍛 {usage}%";
              format-alt = "{icon0}{icon1}{icon2}{icon3}";
              format-icons = ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"];
            };

            "memory" = {
              interval = 30;
              format = "󰾆 {percentage}%";
              format-alt = "󰾅 {used}GB";
              max-length = 10;
              tooltip = true;
              tooltip-format = " {used:.1f}GB/{total:.1f}GB";
            };

            "backlight" = {
              format = "{icon} {percent}%";
              format-icons = ["" "" "" "" "" "" "" "" ""];
              on-scroll-up = "${pkgs.brightnessctl}/bin/brightnessctl set 2%+";
              on-scroll-down = "${pkgs.brightnessctl}/bin/brightnessctl set 2%-";
            };

            "network" = {
              # on-click = "nm-connection-editor";
              # "interface" = "wlp2*"; # (Optional) To force the use of this interface
              format-wifi = "󰤨 Wi-Fi";
              # format-wifi = " {bandwidthDownBits}  {bandwidthUpBits}";
              # format-wifi = "󰤨 {essid}";
              format-ethernet = "󱘖 Wired";
              # format-ethernet = " {bandwidthDownBits}  {bandwidthUpBits}";
              format-linked = "󱘖 {ifname} (No IP)";
              format-disconnected = "󰤮 Off";
              # format-disconnected = "󰤮 Disconnected";
              format-alt = "󰤨 {signalStrength}%";
              tooltip-format = "󱘖 {ipaddr}  {bandwidthUpBytes}  {bandwidthDownBytes}";
            };

            "bluetooth" = {
              format = "";
              # format-disabled = ""; # an empty format will hide the module
              format-connected = " {num_connections}";
              tooltip-format = " {device_alias}";
              tooltip-format-connected = "{device_enumerate}";
              tooltip-format-enumerate-connected = " {device_alias}";
              on-click = "blueman-manager";
            };

            "pulseaudio" = {
              format = "{icon} {volume}";
              format-muted = " ";
              on-click = "pavucontrol -t 3";
              tooltip-format = "{icon} {desc} // {volume}%";
              scroll-step = 4;
              format-icons = {
                headphone = "";
                hands-free = "";
                headset = "";
                phone = "";
                portable = "";
                car = "";
                default = ["" "" ""];
              };
            };

            "pulseaudio#microphone" = {
              format = "{format_source}";
              format-source = " {volume}%";
              format-source-muted = "";
              on-click = "pavucontrol -t 4";
              tooltip-format = "{format_source} {source_desc} // {source_volume}%";
              scroll-step = 5;
            };

            "tray" = {
              icon-size = 12;
              spacing = 5;
            };

            "battery" = {
              states = {
                good = 95;
                warning = 30;
                critical = 20;
              };
              format = "{icon} {capacity}%";
              # format-charging = " {capacity}%";
              format-charging = " {capacity}%";
              format-plugged = " {capacity}%";
              format-alt = "{time} {icon}";
              format-icons = ["󰂎" "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
            };

            "custom/power" = {
              format = "{}";
              on-click = "wlogout -b 4";
              interval = 86400; # once every day
              tooltip = true;
            };
          }
        ];
        style = ''
         
         /* Global Styles */
         * {
             font-family: "JetBrainsMono Nerd Font", "Fira Sans", sans-serif;
             font-size: 14px;
             font-feature-settings: '"zero", "ss01", "ss02", "ss03", "ss04", "ss05", "cv31"';
             margin: 0;
             padding: 0;
         }

         /* Color Palette */
         @define-color base       #2E3440;
         @define-color mantle     #3B4252;
         @define-color crust      #434C5E;
         @define-color text       #D8DEE9;
         @define-color subtext0   #E5E9F0;
         @define-color subtext1   #ECEFF4;
         @define-color surface0   #4C566A;
         @define-color surface1   #616E88;
         @define-color surface2   #7B849C;
         @define-color overlay0   #81A1C1;
         @define-color overlay1   #88C0D0;
         @define-color overlay2   #8FBCBB;
         @define-color blue       #81A1C1;
         @define-color lavender   #B48EAD;
         @define-color sapphire   #5E81AC;
         @define-color sky        #88C0D0;
         @define-color teal       #8FBCBB;
         @define-color green      #A3BE8C;
         @define-color yellow     #EBCB8B;
         @define-color peach      #D08770;
         @define-color maroon     #BF616A;
         @define-color red        #BF616A;
         @define-color mauve      #B48EAD;
         @define-color pink       #D08770;
         @define-color flamingo   #D8DEE9;
         @define-color rosewater  #ECEFF4;

         /* Waybar Window */
         window#waybar {
             background: transparent;
             border-radius: 10px;
             box-shadow: 0 0 10px rgba(0, 0, 0, 0.5);
             transition: background-color 0.5s ease;
         }

         /* Tooltip */
         tooltip {
             background: @base;
             border-radius: 8px;
             box-shadow: 0 0 5px rgba(0, 0, 0, 0.3);
         }

         tooltip label {
             color: @text;
             padding: 5px;
         }

         /* Modules */
         /* Removed unsupported 'display: flex' */
         .modules-left, .modules-center, .modules-right {
             padding-left: 10px;
             padding-right: 10px;
             background: @base;
             border-radius: 10px;
         }

         /* Module Styling */
#battery {
             color: @green;
         }

#battery.critical {
             background-color: @red;
             color: @text;
             animation-name: blink;
             animation-duration: 0.5s;
             animation-timing-function: linear;
             animation-iteration-count: infinite;
             animation-direction: alternate;
         }

         @keyframes blink {
             to {
                 color: @surface0;
             }
         }

         /* Workspaces */
#workspaces button {
             padding: 5px 10px;
             margin: 0 5px;
             border-radius: 10px;
             background: @surface1;
             color: @text;
             transition-property: background-color;
             transition-duration: 0.3s;
             transition-timing-function: ease;
         }

#workspaces button.active {
             background: @blue;
             color: @text;
         }

#workspaces button:hover {
             background: @overlay0;
         }

         /* Custom Modules */
#custom-weather {
             color: @lavender;
         }

#custom-lock {
             color: @maroon;
         }

         /* Tray */
#tray {
             box-shadow: inset 0 -3px @blue;
         }

         /* Sliders */
#pulseaudio-slider, #backlight-slider {
             min-width: 100px;
             min-height: 5px;
             border-radius: 5px;
             background: @surface1;
         }

#pulseaudio-slider highlight, #backlight-slider highlight {
             border-radius: 5px;
             background: @blue;
         }
          @define-color base       #2E3440;
          @define-color mantle     #3B4252;
          @define-color crust      #434C5E;
          @define-color text       #D8DEE9;
          @define-color subtext0   #E5E9F0;
          @define-color subtext1   #ECEFF4;
          @define-color surface0   #4C566A;
          @define-color surface1   #616E88;
          @define-color surface2   #7B849C;
          @define-color overlay0   #81A1C1;
          @define-color overlay1   #88C0D0;
          @define-color overlay2   #8FBCBB;
          @define-color blue       #81A1C1;
          @define-color lavender   #B48EAD;
          @define-color sapphire   #5E81AC;
          @define-color sky        #88C0D0;
          @define-color teal       #8FBCBB;
          @define-color green      #A3BE8C;
          @define-color yellow     #EBCB8B;
          @define-color peach      #D08770;
          @define-color maroon     #BF616A;
          @define-color red        #BF616A;
          @define-color mauve      #B48EAD;
          @define-color pink       #D08770;
          @define-color flamingo   #D8DEE9;
          @define-color rosewater  #ECEFF4;

          /* Waybar Window */
          window#waybar {
              background: transparent;
              border-radius: 10px;
              box-shadow: 0 0 10px rgba(0, 0, 0, 0.5);
              transition: background-color 0.5s ease;
              margin: 0;
              padding: 0;
          }

          /* Tooltip */
          tooltip {
              background: @base;
              border-radius: 8px;
              box-shadow: 0 0 5px rgba(0, 0, 0, 0.3);
          }

          tooltip label {
              color: @text;
              padding: 5px;
          }

          /* Modules */
          .modules-left, .modules-center, .modules-right {
              padding: 0 10px;
              background: @base;
              border-radius: 10px;
          }

          /* Module Styling */
#battery {
              color: @green;
          }

#battery.critical {
              background-color: @red;
              color: @text;
              animation: blink 0.5s linear infinite alternate;
          }

          @keyframes blink {
              to {
                  color: @surface0;
              }
          }

          /* Workspaces */
#workspaces button {
              padding: 5px 10px;
              margin: 0 5px;
              border-radius: 10px;
              background: @surface1;
              color: @text;
              transition: background-color 0.3s ease;
          }

#workspaces button.active {
              background: @blue;
              color: @text;
          }

#workspaces button:hover {
              background: @overlay0;
          }

          /* Custom Modules */
#custom-weather {
              color: @lavender;
          }

#custom-lock {
              color: @maroon;
          }

          /* Tray */
#tray {
              box-shadow: inset 0 -3px @blue;
          }

          /* Sliders */
#pulseaudio-slider, #backlight-slider {
              width: 100px;
              height: 5px;
              border-radius: 5px;
              background: @surface1;
          }

#pulseaudio-slider highlight, #backlight-slider highlight {
              border-radius: 5px;
              background: @blue;
          }
        '';
      };
    })
  ];
}
