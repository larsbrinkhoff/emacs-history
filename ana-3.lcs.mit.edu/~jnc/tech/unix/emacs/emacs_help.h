/* character help explanations */

/* note -- the text of the help instructions is kept in helpstrings
 * and helpfile in uncompiled and compiled format.  make_help is the
 * compiler for the helpstrings file. */

#define HINSERT 1		/* index of self inserting chars */

char *hdate = "07/07/80";
int helfile = 0;
#define HELSIZE 128

/* ****** NOTE -- it is no longer necessary to change the pathnames. */
/* EMACS will put its error messages, help messages, and statistics in */
/* the directory where the sources are if you don't modify these definitions */

char *helname = SDIR/helpfile";
char *statpath = SDIR/s%s";
char *errpath = SDIR/errfile";
char *termdir = SDIR/terminals/%s";
char helpc[NCHARS] = {
	41,	2,	3,	4,	5,	6,	7,	8,
	44,	9,	10,	11,	12,	10,	13,	14,
	15,	16,	17,	18,	19,	20,	21,	22,
	23,	24,	25,	89,	0,	100,	90,	0,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	1,
	1,	1,	1,	1,	1,	1,	1,	44,

/* META characters */

	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	65,	64,	0,	0,
	0,	83,	67,	66,	0,	0,	0,	0,
	111,	0,	0,	0,	0,	106,	0,	0,
	41,	43,	79,	0,	61,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	49,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	105,	0,	34,	0,	35,	42,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	69,	0,	0,	60,
	0,	52,	26,	27,	28,	53,	29,	48,
	0,	0,	0,	0,	0,	50,	0,	0,
	71,	68,	30,	73,	46,	0,	31,	72,
	98,	74,	32,	87,	0,	88,	104,	33,

/* control - x characters */

	0,	0,	112,	36,	62,	93,	55,	0,
	0,	63,	0,	56,	97,	51,	108,	58,
	0,	84,	37,	38,	47,	102,	110,	39,
	40,	0,	113,	0,	0,	0,	101,	0,
	0,	103,	0,	0,	0,	96,	99,	0,
	0,	0,	0,	107,	0,	95,	0,	0,
	0,	57,	59,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	86,	109,	94,	0,
	0,	0,	91,	0,	0,	0,	92,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	85,	0,
	0,	0,	54,	0,	80,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	82,	0,	81,	0,
};
