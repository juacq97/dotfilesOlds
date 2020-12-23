/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */
static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom     */
static int fuzzy = 1;                      /* -F  option; if 0, dmenu doesn't use fuzzy matching     */
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] = {
	"Futura Bk Bt:Book:size=10",
	"JoyPixels:pixelsize=8:antialias=true:autohint=true"
};
static const char *prompt      = NULL;      /* -p  option; prompt to the left of input field */

static const char *colors[SchemeLast][2] = {
	/*     fg         bg       */
	[SchemeNorm] = { "#fbf2c7", "#1d2021" },
	[SchemeSel] = { "#1d2021", "#fbf2c7" },
	[SchemeSelHighlight] = { "#fb4934", "#fbf2c7" },
	[SchemeNormHighlight] = { "#fb4934", "#1d2021" },
	[SchemeOut] = { "#e4ecec", "#D2E1DD" },
};
/* -l option; if nonzero, dmenu uses vertical list with given number of lines */
static unsigned int lines      = 0;
static unsigned int lineheight = 25;         /* -h option; minimum height of a menu line     */

/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";
