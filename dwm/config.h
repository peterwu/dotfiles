/* See LICENSE file for copyright and license details. */

#include <X11/XF86keysym.h>

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "Noto:size=10" };
static const char dmenufont[]       = "Roboto:size=10";
static const char col_gray1[]       = "#002b36";
static const char col_gray2[]       = "#073642";
static const char col_gray3[]       = "#fdf6e3";
static const char col_gray4[]       = "#fdf6e3";
static const char col_cyan[]        = "#073642";

static const char *colors[][3]      = {
	/*                 fg         bg         border   */
	[SchemeNorm]   = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]    = { col_gray4, col_cyan,  col_cyan  },
	[SchemeTitle]  = { col_gray4, col_cyan,  col_cyan  },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class                instance    title       tags mask     isfloating   monitor */
	{ "st-256color",        NULL,       NULL,       1,            0,           -1 },
	{ "Firefox",            NULL,       NULL,       1<<1,         0,           -1 },
	{ "Google-chrome",      NULL,       NULL,       1<<1,         0,           -1 },
	{ "Emacs",              NULL,       NULL,       1<<2,         0,           -1 },
	{ "Thunderbird",        NULL,       NULL,       1<<3,         0,           -1 },
	{ "libreoffice-writer", NULL,       NULL,       1<<4,         0,           -1 },
	{ "vlc",                NULL,       NULL,       1<<5,         0,           -1 },
	{ "Virt-manager",       NULL,       NULL,       1<<6,         0,           -1 },
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
static const char *dmenucmd[]       = { "rofi", "-show", NULL};
static const char *termcmd[]        = { "st", NULL };
static const char *prevcmd[]        = { "playerctl", "previous", NULL };
static const char *nextcmd[]        = { "playerctl", "next", NULL };
static const char *playcmd[]        = { "playerctl", "play", NULL };
static const char *pausecmd[]       = { "playerctl", "pause", NULL };
static const char *stopcmd[]        = { "playerctl", "stop", NULL };
static const char *browsercmd[]     = { "google-chrome-stable", NULL };
static const char *mailcmd[]        = { "thunderbird", NULL };
static const char *vlccmd[]         = { "vlc", NULL };
static const char *slockcmd[]       = { "slock", NULL };
static const char *brightupcmd[]    = { "light", "-A", "5", NULL };
static const char *brightdowncmd[]  = { "light", "-U", "5", NULL };
static const char *volupcmd[]       = { "pactl", "set-sink-volume", "0", "+5%", NULL };
static const char *voldowncmd[]     = { "pactl", "set-sink-volume", "0", "-5%", NULL };
static const char *volmutecmd[]     = { "pactl", "set-sink-mute", "0", "toggle", NULL };
static const char *micmutecmd[]     = { "pactl", "set-source-mute", "1", "toggle", NULL };

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY|ShiftMask,             XK_f,      spawn,          {.v = browsercmd } },
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
	{ MODKEY|ShiftMask,             XK_l,      spawn,          {.v = slockcmd } },
	{ 0,                      XF86XK_HomePage, spawn,          {.v = browsercmd } },
	{ 0,                          XF86XK_Mail, spawn,          {.v = mailcmd } },
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
