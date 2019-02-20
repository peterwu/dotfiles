/* user and group to drop privileges to */
static const char *user  = "nobody";
static const char *group = "nobody";

static const char *colorname[NUMCOLS] = {
	[INIT] =   "#002b36",   /* after initialization */
	[INPUT] =  "#268bd2",   /* during input */
	[FAILED] = "#cb4b16",   /* wrong password */
	[CAPS] =   "#dc322f",   /* CapsLock on */
};

/* treat a cleared input like a wrong password (color) */
static const int failonclear = 1;

/* time in seconds before the monitor shuts down */
static const int monitortime = 9;

/* time to cancel lock with mouse movement in seconds */
static const int timetocancel = 3;
