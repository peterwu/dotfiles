/* See LICENSE file for copyright and license details. */

#include <X11/XF86keysym.h>

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "Roboto:pixelsize=14", "Font Awesome 5 Free:pixelsize=14", "Noto:pixelsize=14",};
static const char dmenufont[]       = "Roboto:pixelsize=28";

/* solarized color palette */
static const char col_base03[]      = "#002b36";
static const char col_base02[]      = "#073642";
static const char col_base01[]      = "#586e75";
static const char col_base00[]      = "#657b83";
static const char col_base0[]       = "#839496";
static const char col_base1[]       = "#93a1a1";
static const char col_base2[]       = "#eee8d5";
static const char col_base3[]       = "#fdf6e3";
static const char col_yellow[]      = "#b58900";
static const char col_orange[]      = "#cb4616";
static const char col_red[]         = "#dc322f";
static const char col_magenta[]     = "#d33682";
static const char col_violet[]      = "#6c71c4";
static const char col_blue[]        = "#268bd2";
static const char col_cyan[]        = "#2aa198";
static const char col_green[]       = "#859900";

static const char *colors[][3]      = {
	/*                 fg           bg           border    */
	[SchemeNorm]   = { col_base3,   col_base03,  col_base02 },
	[SchemeSel]    = { col_magenta, col_base03,  col_base02 },
	[SchemeTitle]  = { col_base3,   col_base03,  col_base02 },
};

/* tagging */
static const char *tags[] = { "", "", "", "", "" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class                instance    title       tags mask     isfloating   monitor */
	{ "st-256color",        NULL,       NULL,       1,            0,           -1 },
	{ "Firefox",            NULL,       NULL,       1<<1,         0,           -1 },
	{ "Chromium",           NULL,       NULL,       1<<1,         0,           -1 },
	{ "Google-chrome",      NULL,       NULL,       1<<1,         0,           -1 },
	{ "Emacs",              NULL,       NULL,       1<<2,         0,           -1 },
	{ "Thunderbird",        NULL,       NULL,       1<<3,         0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[]       = { "rofi", "-show", "combi", NULL};
static const char *termcmd[]        = { "st", NULL };
static const char *prevcmd[]        = { "playerctl", "previous", NULL };
static const char *nextcmd[]        = { "playerctl", "next", NULL };
static const char *playcmd[]        = { "playerctl", "play", NULL };
static const char *pausecmd[]       = { "playerctl", "pause", NULL };
static const char *stopcmd[]        = { "playerctl", "stop", NULL };
static const char *firefoxcmd[]     = { "firefox", NULL };
static const char *chromecmd[]      = { "google-chrome-stable", NULL };
static const char *emacscmd[]       = { "emacs", "-mm", NULL };
static const char *mailcmd[]        = { "thunderbird", NULL };
static const char *vlccmd[]         = { "vlc", NULL };
static const char *slockcmd[]       = { "slock", NULL };
static const char *prtsccmd[]    = { "scrot", "%Y-%m-%d_$wx$h-scrot.png", "-e", "mv $f ~/Pictures/", NULL };
static const char *brightupcmd[]    = { "light", "-A", "5", NULL };
static const char *brightdowncmd[]  = { "light", "-U", "5", NULL };
static const char *volupcmd[]       = { "pactl", "set-sink-volume", "0", "+5%", NULL };
static const char *voldowncmd[]     = { "pactl", "set-sink-volume", "0", "-5%", NULL };
static const char *volmutecmd[]     = { "pactl", "set-sink-mute", "0", "toggle", NULL };
static const char *micmutecmd[]     = { "pactl", "set-source-mute", "1", "toggle", NULL };
static const char *restartcmd[]     = { "systemctl", "reboot", NULL };
static const char *shutdowncmd[]    = { "systemctl", "poweroff", NULL };

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_r,      spawn,          {.v = dmenucmd } },
	{ MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY|ShiftMask,             XK_f,      spawn,          {.v = firefoxcmd } },
	{ MODKEY|ShiftMask,             XK_g,      spawn,          {.v = chromecmd } },
	{ MODKEY|ShiftMask,             XK_e,      spawn,          {.v = emacscmd } },
	{ MODKEY|ShiftMask,             XK_t,      spawn,          {.v = mailcmd } },
	{ MODKEY|ShiftMask,             XK_v,      spawn,          {.v = vlccmd } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY|ShiftMask,             XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	/* custom key binds */
	{ MODKEY|ShiftMask,                  XK_l, spawn,          {.v = slockcmd } },
	{ 0,                          XF86XK_Mail, spawn,          {.v = mailcmd } },
	{ 0,                             XK_Print, spawn,          {.v = prtsccmd } },
	{ 0,               XF86XK_MonBrightnessUp, spawn,          {.v = brightupcmd } },
	{ 0,             XF86XK_MonBrightnessDown, spawn,          {.v = brightdowncmd } },
	{ 0,              XF86XK_AudioLowerVolume, spawn,          {.v = voldowncmd } },
	{ 0,              XF86XK_AudioRaiseVolume, spawn,          {.v = volupcmd } },
	{ 0,                     XF86XK_AudioMute, spawn,          {.v = volmutecmd } },
	{ 0,                  XF86XK_AudioMicMute, spawn,          {.v = micmutecmd } },
	{ 0,                     XF86XK_AudioPrev, spawn,          {.v = prevcmd } },
	{ 0,                     XF86XK_AudioNext, spawn,          {.v = nextcmd } },
	{ 0,                     XF86XK_AudioPlay, spawn,          {.v = playcmd } },
	{ 0,                    XF86XK_AudioPause, spawn,          {.v = pausecmd } },
	{ 0,                     XF86XK_AudioStop, spawn,          {.v = stopcmd } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
	{ MODKEY|ControlMask|ShiftMask, XK_q,      quit,           {1} }, 
	{ MODKEY|ControlMask|ShiftMask, XK_r,      spawn,          {.v = restartcmd } }, 
	{ MODKEY|ControlMask|ShiftMask, XK_x,      spawn,          {.v = shutdowncmd } }, 
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
