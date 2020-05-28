//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
	{"  ",	    "date +'%b %d, %R' ", 1, 0},
	{"   " , "~/.local/bin/status-bar/volume ", 0, 2},
	{"   ", "echo $(cat /sys/class/power_supply/BAT0/capacity)% ", 60, 0},
	{" ", "~/.local/bin/status-bar/red", 30, 0},
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim = ' ';
