
# line 2 "commands.y"

/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)commands.y 1.9 8/17/83";

/*
 * Yacc grammar for debugger commands.
 */

#include "defs.h"
#include "symbols.h"
#include "operators.h"
#include "tree.h"
#include "process.h"
#include "source.h"
#include "scanner.h"
#include "names.h"
#include "lists.h"

private String curformat = "X";


# line 44 "commands.y"
typedef union  {
    Name y_name;
    Symbol y_sym;
    Node y_node;
    Integer y_int;
    Operator y_op;
    long y_long;
    double y_real;
    String y_string;
    Boolean y_bool;
    Cmdlist y_cmdlist;
    List y_list;
} YYSTYPE;
# define ALIAS 257
# define AND 258
# define ASSIGN 259
# define AT 260
# define CALL 261
# define CATCH 262
# define CONT 263
# define DEBUG 264
# define DELETE 265
# define DIV 266
# define DUMP 267
# define EDIT 268
# define FILE 269
# define FUNC 270
# define GRIPE 271
# define HELP 272
# define IF 273
# define IGNORE 274
# define IN 275
# define LIST 276
# define MOD 277
# define NEXT 278
# define NEXTI 279
# define NIL 280
# define NOT 281
# define OR 282
# define PRINT 283
# define PSYM 284
# define QUIT 285
# define RUN 286
# define SH 287
# define SKIP 288
# define SOURCE 289
# define STATUS 290
# define STEP 291
# define STEPI 292
# define STOP 293
# define STOPI 294
# define TRACE 295
# define TRACEI 296
# define USE 297
# define WHATIS 298
# define WHEN 299
# define WHERE 300
# define WHEREIS 301
# define WHICH 302
# define INT 303
# define REAL 304
# define NAME 305
# define STRING 306
# define LFORMER 307
# define RFORMER 308
# define ABSTRACTION 309
# define ARROW 310
# define REDIRECT 311
# define UNARYSIGN 312
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 41,
	10, 44,
	59, 44,
	-2, 42,
-1, 125,
	275, 0,
	-2, 169,
-1, 254,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 127,
-1, 257,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 129,
-1, 259,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 131,
-1, 281,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 128,
-1, 282,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 133,
-1, 283,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 130,
-1, 284,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 132,
-1, 285,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 134,
	};
# define YYNPROD 197
# define YYLAST 1745
short yyact[]={

  64, 274, 273, 133, 269, 134, 236,  63, 131, 183,
 132, 166, 152, 119, 115, 114, 113, 214, 177, 140,
 165,  62,  50, 188, 174, 217, 255, 256, 181, 182,
 140, 277, 213, 193, 122,  60, 118, 261, 290, 172,
 164, 162, 160, 163,  57, 159, 288, 244, 206, 221,
  64, 146, 147, 133, 164, 134, 116,  63, 131, 161,
 132, 138, 155, 143, 278, 123, 222, 158, 177, 173,
 117, 268, 176,   6, 174, 267, 260,   5, 169,  41,
 121, 164, 162,  21, 163,   3, 276,   2, 148,   1,
 218, 265, 137,  58,  18,   7, 130, 241, 142,  34,
  64,  24,  67, 133,  59, 134,  25,  63, 131, 167,
 132,  26,   0,   0,   0,   0,   0, 171,   0, 173,
   0,   0, 176,   0,   0,   0, 258,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0, 184,   0,   0,   0,
  64, 185,   0, 133,   0, 134,   0,  63, 131,   0,
 132,   0,   0,   0, 216,   0,   0,   0,   0,   0,
   0,   0, 211,   0,   0,   0,   0,   0,   0,   0,
 227,   0,   0, 228,   0, 231, 232, 233,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
  64,   0,   0, 133,   0, 134, 157,  63, 131,  54,
 132,  56, 266,  55, 157, 245,   0,   0,   0,   0,
   0,   0,  68,  69,  70,  71,  72,  73,  74,  75,
  76,  77,  78,  79,  80,  81,  82,  83,   0,  84,
  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,
  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
 105, 106, 107, 108, 109, 110, 111, 112, 135, 136,
  66, 145,  68,  69,  70,  71,  72,  73,  74,  75,
  76,  77,  78,  79,  80,  81,  82,  83, 175,  84,
  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,
  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
 105, 106, 107, 108, 109, 110, 111, 112, 135, 136,
  66, 145,  68,  69,  70,  71,  72,  73,  74,  75,
  76,  77,  78,  79,  80,  81,  82,  83, 175,  84,
  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,
  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
 105, 106, 107, 108, 109, 110, 111, 112, 135, 136,
  66, 145,  68,  69,  70, 126,  72,  73,  74,  75,
  76,  77,  78,  79,  80,  81,  82,  83, 140,  84,
 125,  86,  87,  88,  89,  90,  91,  92,  93,  94,
  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
 105, 106, 107, 108, 109, 110, 111, 112, 135, 136,
  66, 128,  68,  69,  70, 126,  72,  73,  74,  75,
  76,  77,  78,  79,  80,  81,  82,  83, 124,  84,
 125,  86,  87,  88,  89,  90,  91,  92,  93,  94,
  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
 105, 106, 107, 108, 109, 110, 111, 112, 135, 136,
  66, 128,  64, 156,  53, 133, 192, 134, 287,  63,
 131, 156, 132,   0,   0,   0,   0, 223,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 225,   0, 226,   0,   0,
   0,   0,  64,   0,   0, 133,   0, 134,   0,  63,
 131,   0, 132,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,  64,   0,   0,   0,   0, 179,   0,  63,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,  64,   0,   0,   0,   0,   0,   0,  63,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0, 157,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,  68,  69,  70,  71,  72,  73,
  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,
 272,  84,  85,  86,  87,  88,  89,  90,  91,  92,
  93,  94,  95,  96,  97,  98,  99, 100, 101, 102,
 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
 135, 136,  66, 145,  68,  69,  70, 126,  72,  73,
  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,
 224,  84, 125,  86,  87,  88,  89,  90,  91,  92,
  93,  94,  95,  96,  97,  98,  99, 100, 101, 102,
 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
 135, 136,  66, 145,  68,  69,  70,  71,  72,  73,
  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,
   0,  84,  85,  86,  87,  88,  89,  90,  91,  92,
  93,  94,  95,  96,  97,  98,  99, 100, 101, 102,
 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
 229, 149,  66,  68,  69,  70,  71,  72,  73,  74,
  75,  76,  77,  78,  79,  80,  81,  82,  83,   0,
  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,
  94,  95,  96,  97,  98,  99, 100, 101, 102, 103,
 104, 105, 106, 107, 108, 109, 110, 111, 112,   0,
   0,  66,  68,  69,  70,  71,  72,  73,  74,  75,
  76,  77,  78,  79,  80,  81,  82,  83, 139,  84,
  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,
  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
 105, 106, 107, 108, 109, 110, 111, 112, 156,  64,
  66,   0,   0,   0,   0,   0,  63,  68,  69,  70,
  71,  72,  73,  74,  75,  76,  77,  78,  79,  80,
  81,  82,  83,   0,  84,  85,  86,  87,  88,  89,
  90,  91,  92,  93,  94,  95,  96,  97,  98,  99,
 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
 110, 111, 112,   0,  54,  66,  56,   0,  55,   0,
 129,   0,   0,  51,   0,   0, 234,   0,   0,  61,
   0,   0,   0,   4,   0, 238,   0,   0,   0, 242,
 120,   0,   0,   0,   0,   0,   0,   0, 141,   0,
   0, 186, 187,   0,   0,   0, 151,   0,   0,   0,
   0,  54,   0,  56,   0,  55, 212, 215,   0,   0,
  51,   0,   0,   0,   0, 168,   0,   0,   0,   0,
   0, 270,   0,   0, 178, 180,   0,   0, 275,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0, 286,   0,   0,   0,   0,   0,   0,  54,   0,
  56,   0,  55,   0,   0,   0,   0,  51,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 289,   0,   0,
 264,   0,   0,   0,   0,   0, 190,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,  68,  69,  70,  71,  72,  73,  74,  75,  76,
  77,  78,  79,  80,  81,  82,  83,   0,  84,   0,
  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
  96,  97,  98,  99, 100, 101, 102, 103, 104, 105,
 106, 107, 108, 109, 110, 111, 112,   0,   0,  66,
   0,   0,   0,  39, 190,   8,   0,  35,   9,  10,
  36,  11,   0,  37,  12,  13,  14,  15,  16,   0,
  17,   0,  40, 196,  44,  45,   0,   0, 197,  32,
  19,  20,  52,  22,   0,  23,  38,  42,  43,  46,
  47,  48,  49,  31,  27,  28,  33,  29,  30,  53,
  39,   0,   8,   0,  35,   9,  10,  36,  11, 191,
  37,  12,  13,  14,  15,  16,   0,  17,   0,  40,
   0,  44,  45, 193,   0,   0,  32,  19,  20,  52,
  22,   0,  23,  38,  42,  43,  46,  47,  48,  49,
  31,  27,  28,  33,  29,  30,  53,  39,   0,   8,
 154,  35,   9,  10,  36,  11,   0,  37,  12,  13,
  14,  15,  16,   0,  17,   0,  40,  65,  44,  45,
   0,   0,   0,  32,  19,  20,  52,  22,   0,  23,
  38,  42,  43,  46,  47,  48,  49,  31,  27,  28,
  33,  29,  30,  53, 205,   0,   0,   0,   0,   0,
   0, 205, 279, 196, 194,   0, 195, 153, 197, 263,
 196, 194,   0, 195,   0, 197,   0,   0,   0,   0,
   0, 202, 204, 203,   0,   0,   0, 205, 202, 204,
 203,   0,   0,   0, 205, 235, 196, 194,   0, 195,
   0, 197,   0, 196, 194, 219, 195,   0, 197,   0,
   0,   0,   0, 193, 202, 204, 203,   0,   0,   0,
 193, 202, 204, 203,   0,   0,   0, 205,   0, 196,
 194,   0, 195,   0, 197,   0, 196, 194,   0, 195,
   0, 197,   0,   0,   0,   0, 193,   0,   0, 200,
   0,   0,   0, 193, 202, 204, 203, 198,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 199,   0,
   0,   0,   0,   0,   0,   0, 262,   0,   0, 193,
   0, 220, 150,   0,   0,   0, 193,   0,   0, 230,
   0, 271,   0, 230,   0,   0,   0,   0,   0,   0,
   0,   0, 239, 240,   0,   0,   0,   0, 127, 127,
   0, 144,   0,   0, 280,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 170,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 230,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0, 200,
   0,   0,   0,   0,   0,   0, 200, 198,   0,   0,
   0,   0,   0,   0, 198,   0,   0,   0, 199,   0,
   0,   0,   0, 201, 230, 199,   0, 189,   0,   0,
 201,   0, 200,   0, 207, 208, 209, 210,   0, 200,
 198,   0,   0, 189,   0,   0,   0, 198,   0,   0,
   0, 199,   0,   0,   0,   0, 201,   0, 199,   0,
   0,   0,   0, 201,   0, 200,   0,   0,   0,   0,
   0,   0, 200, 198,   0, 237,   0,   0,   0,   0,
 198,   0, 243,   0, 199,   0,   0,   0,   0, 201,
   0, 199,   0,   0,   0,   0, 201, 246, 247, 248,
 249, 250, 251, 252, 253, 254, 257, 259,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 281, 282,
   0, 283,   0, 284, 285 };
short yypact[]={

-1000,1003,-1000,  34,-1000,-1000, -27,-1000, 586,-287,
-288,-289,-1000,-1000, 690,-1000,-1000,-290,-1000, 586,
-1000,-1000,-1000,-1000,-1000, 165, 115, 586, 487, 690,
 690,-1000, 437,-1000,-1000, 586,-291,-1000,-1000, 690,
 635,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
  -2,-292,-1000,-1000, 586, 171, 437,-1000,-1000,-1000,
-1000, -22,-1000, 537, 586,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-296,-296,-1000,-1000,
  28,-296,-254,-254, 437, 904, 170,1394, -10,  28,
-1000, 437, 437, 437, 437,-1000,-1000,-243,-254,-1000,
 437,  28, -98,-1000,1394,-1000,-1000,-1000,-1000,-1000,
1361,  28,-1000, 690,   5,-1000,-1000,-1000, 455,-292,
 171, 690, 171, 171, 171, 690,-1000,-1000,  28,  12,
1354,-299, 437, 437, 690, 690,-1000, 437,  28, 437,
  28,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,1394,
  28,-1000, -11, 690, 437, 437, 437, 437, 437, 437,
 437, 437, -35,  65,  15, -24, 178,1191,1191,1191,
1328,-254,-1000, 586, 170,-1000,-1000,1050,-301, 437,
-1000, 178, 455,-1000,-1000,-303,-304, 690,  39,-1000,
-1000,  12,  12,-1000,-1000,-1000,-1000,1394, -62,-1000,
-1000,  23,-1000,1321, 178,-1000,1191,1191, -59, -59,
 -59, -59, -59,1191,1387, 437, 437,1387, 437,1387,
 437, 437,-1000,-1000,-1000, 956, -13,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000, 690,-1000,-1000,-1000,
-1000,1387,1387,1387,1387,1387,-1000, -21,-1000,-1000,
-1000 };
short yypgo[]={

   0, 111, 106,  20, 104,  29,  28, 840,1327, 102,
  21,  75,  71, 212, 101,  65,  34,  99,  98,  97,
 908, 841,1482,1000,  23,  96,  22,  95,  94,1269,
  91,  90,  89,  87,  85,  56,  83,  79,  67,  66,
 487 };
short yyr1[]={

   0,  32,  32,  33,  33,  33,  34,  34,   4,   4,
  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,
  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,
  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,
  11,  11,  38,  36,  36,  37,  39,  39,  40,  40,
  40,  14,  14,  14,  14,  35,  31,  31,  18,  18,
  30,  30,  13,  13,  12,  12,  12,  12,  12,  12,
  12,  27,  27,  27,   1,   1,   2,   2,  15,  15,
  16,  16,  16,   5,   6,   6,  19,  19,  28,  28,
  28,  28,  29,  29,  17,  17,  17,  17,  26,  26,
  26,  26,  26,  26,  26,   3,   3,   7,   7,  20,
  20,  21,  21,  22,  22,  22,  22,  22,  22,  22,
  22,  22,  22,  22,  22,  22,  22,  22,  22,  22,
  22,  22,  22,  22,  22,  22,  23,  23,  23,  23,
  23,  23,  23,  23,  23,  24,  25,  25,  25,  10,
   8,   8,   9,   9,   9,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9 };
short yyr2[]={

   0,   2,   0,   2,   2,   1,   1,   2,   3,   0,
   1,   4,   2,   1,   2,   2,   3,   3,   1,   2,
   1,   1,   2,   1,   2,   1,   1,   1,   3,   1,
   3,   3,   3,   4,   3,   3,   2,   2,   5,   2,
   2,   3,   0,   3,   1,   2,   2,   1,   1,   2,
   2,   1,   1,   1,   1,   0,   2,   0,   1,   1,
   3,   2,   1,   1,   2,   1,   1,   2,   2,   1,
   1,   3,   2,   1,   1,   1,   1,   1,   1,   3,
   2,   2,   4,   1,   0,   1,   1,   0,   1,   2,
   4,   2,   1,   1,   4,   5,   3,   3,   1,   2,
   3,   3,   3,   2,   3,   0,   1,   1,   0,   0,
   2,   1,   3,   1,   1,   3,   2,   2,   2,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   4,   3,
   4,   3,   4,   4,   4,   3,   1,   4,   3,   3,
   2,   4,   2,   2,   4,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1 };
short yychk[]={

-1000, -32, -33, -34,  10, -11, -12, -27, 259, 262,
 263, 265, 268, 269, 270, 271, 272, 274, -28, 284,
 285, -36, 287, 289, -14,  -2,  -1, 298, 299, 301,
 302, 297, 283, 300, -17, 261, 264, 267, 290, 257,
 276, -37, 291, 292, 278, 279, 293, 294, 295, 296,
 -26,  47, 286, 303,  38,  42,  40,  10,  59,  -4,
  62, -23, -10,  42,  35,  -8, 305,  -9, 257, 258,
 259, 260, 261, 262, 263, 264, 265, 266, 267, 268,
 269, 270, 271, 272, 274, 275, 276, 277, 278, 279,
 280, 281, 282, 283, 284, 285, 286, 287, 288, 289,
 290, 291, 292, 293, 294, 295, 296, 297, 298, 299,
 300, 301, 302, 303, 303, 303, -35, -35, -10, 303,
 -23, -35, -16, -15, 273, 275, 260, -22, 306, -23,
 -25,  43,  45,  38,  40, 303, 304, -15, -16, -20,
 273, -23, -18, -16, -22, 306, -10, -10, -35, -21,
 -22, -23, 303,  -8, -29, -10, 303,  36, -38,  47,
  44,  61,  43,  45,  42,  -3, 303, -35, -23, -26,
 -22, -35,  61,  91,  46, 310,  94,  40, -23,  40,
 -23,  -6,  -5, 305,  -6,  -5, -20, -20, -24, -22,
 -23, -29, 306,  92,  43,  45,  42,  47, 266, 277,
 258, 282,  60,  62,  61,  33,  58, -22, -22, -22,
 -22, -16, -20, 275, 260, -20, -24, 123, -31,  44,
  -8,  44, -39, -40, 305,  60,  62,  -3, -26,  -7,
  -8, -26, -26, -26,  -7,  41, 305, -22, -21,  -8,
  -8, -19, -21, -22,  58, -10, -22, -22, -22, -22,
 -22, -22, -22, -22, -22,  61,  62, -22,  61, -22,
  61,  61, -29,  41, -20, -30, -13, -11, -12, 305,
 -21, -29, -40, 305, 305,  -7,  47,  93,  41,  41,
 -29, -22, -22, -22, -22, -22, 125, -13,  59,  -7,
  59 };
short yydef[]={

   2,  -2,   1,   0,   5,   6,   9,  10,   0,   0,
  13,   0,  55,  55,  18,  20,  21,   0,  23,   0,
  25,  26,  27,  55,  29,   0, 109,   0,   0,   0,
   0,  55,   0,  65,  66,   0,   0,  69,  70,  73,
  88,  -2,  51,  52,  53,  54,  76,  77,  74,  75,
   0, 105,  55,  98,   0,   0,   0,   3,   4,   7,
  55,   0, 136,   0,   0, 149, 150, 151, 152, 153,
 154, 155, 156, 157, 158, 159, 160, 161, 162, 163,
 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
 174, 175, 176, 177, 178, 179, 180, 181, 182, 183,
 184, 185, 186, 187, 188, 189, 190, 191, 192, 193,
 194, 195, 196,  12,  14,  15,  84,  84,  19,  22,
  24,   0, 109, 109,   0,  -2, 155,  78, 148, 113,
 114,   0,   0,   0,   0, 146, 147, 109, 109,  36,
   0,  37,   0,  58,  59, 148,  39,  40,  57,  64,
 111,  67,  68,  72,  89,  91,  92,  93,   0, 105,
   0, 108,   0,   0,   0, 108, 106,  45,  99, 103,
   0,   0,   0,   0,   0,   0, 142,  87, 140,   0,
 143,  16,  85,  83,  17,  28,  30,  31,  32, 145,
  80,  81,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 116, 117, 118,
   0, 109,  35,   0,   0,  34, 110,   0,  41,   0,
  71,   0,  43,  47,  48,   0,   0, 108,   0,  97,
 107, 100, 101, 102,  96, 104,   8,  11,   0, 138,
 139,   0,  86,   0,   0, 115, 119, 120, 121, 122,
 123, 124, 125, 126,  -2,   0,   0,  -2,   0,  -2,
   0,   0,  79, 135,  33,   0,   0,  62,  63,  56,
 112,  90,  46,  49,  50,  94, 108, 137, 144, 141,
  82,  -2,  -2,  -2,  -2,  -2,  38,   0,  61,  95,
  60 };
#ifndef lint
static char yaccpar_sccsid[] = "@(#)yaccpar	4.1	(Berkeley)	2/11/83";
#endif not lint

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps> &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 6:
# line 94 "commands.y"
{
	if (yypvt[-0].y_node != nil) {
            if(debug_flag[2]) {dumptree(stderr,yypvt[-0].y_node); fflush (stderr);}
	    eval(yypvt[-0].y_node);
	}
} break;
case 7:
# line 102 "commands.y"
{
	if (yypvt[-1].y_node != nil) {
	    if (yypvt[-0].y_string != nil) {
		setout(yypvt[-0].y_string);
                if(debug_flag[2]) {dumptree(stderr,yypvt[-1].y_node); fflush (stderr);}
		eval(yypvt[-1].y_node);
		unsetout();
	    } else {
                if(debug_flag[2]) {dumptree(stderr,yypvt[-1].y_node); fflush (stderr);}
		eval(yypvt[-1].y_node);
	    }
	}
} break;
case 8:
# line 118 "commands.y"
{
	yyval.y_string = ident(yypvt[-0].y_name);
} break;
case 9:
# line 123 "commands.y"
{
	yyval.y_string = nil;
} break;
case 10:
# line 133 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 11:
# line 138 "commands.y"
{
	yyval.y_node = build(O_ASSIGN, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 12:
# line 143 "commands.y"
{
	yyval.y_node = build(O_CATCH, yypvt[-0].y_long);
} break;
case 13:
# line 148 "commands.y"
{
	yyval.y_node = build(O_CONT, (long) DEFSIG);
} break;
case 14:
# line 153 "commands.y"
{
	yyval.y_node = build(O_CONT, yypvt[-0].y_long);
} break;
case 15:
# line 158 "commands.y"
{
	yyval.y_node = build(O_DELETE, yypvt[-0].y_long);
} break;
case 16:
# line 163 "commands.y"
{
	yyval.y_node = build(O_EDIT, yypvt[-0].y_string);
} break;
case 17:
# line 168 "commands.y"
{
	yyval.y_node = build(O_CHFILE, yypvt[-0].y_string);
} break;
case 18:
# line 173 "commands.y"
{
	yyval.y_node = build(O_FUNC, nil);
} break;
case 19:
# line 178 "commands.y"
{
	yyval.y_node = build(O_FUNC, yypvt[-0].y_node);
} break;
case 20:
# line 183 "commands.y"
{
	yyval.y_node = build(O_GRIPE);
} break;
case 21:
# line 188 "commands.y"
{
	yyval.y_node = build(O_HELP);
} break;
case 22:
# line 193 "commands.y"
{
	yyval.y_node = build(O_IGNORE, yypvt[-0].y_long);
} break;
case 23:
# line 198 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 24:
# line 203 "commands.y"
{
	yyval.y_node = build(O_PSYM, yypvt[-0].y_node);
} break;
case 25:
# line 208 "commands.y"
{
	if (not popinput()) {
	    quit(0);
	} else {
	    yyval.y_node = nil;
	}
} break;
case 26:
# line 217 "commands.y"
{
	run();
	/* NOTREACHED */
} break;
case 27:
# line 223 "commands.y"
{
	shellline();
	yyval.y_node = nil;
} break;
case 28:
# line 229 "commands.y"
{
	yyval.y_node = build(O_SOURCE, yypvt[-0].y_string);
} break;
case 29:
# line 234 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 30:
# line 239 "commands.y"
{
	yyval.y_node = build(yypvt[-2].y_op, nil, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 31:
# line 244 "commands.y"
{
	yyval.y_node = build(yypvt[-2].y_op, yypvt[-1].y_node, nil, yypvt[-0].y_node);
} break;
case 32:
# line 249 "commands.y"
{
	yyval.y_node = build(yypvt[-2].y_op, nil, nil, yypvt[-0].y_node);
} break;
case 33:
# line 254 "commands.y"
{
	yyval.y_node = build(yypvt[-3].y_op, yypvt[-2].y_node, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 34:
# line 259 "commands.y"
{
	yyval.y_node = build(yypvt[-2].y_op, nil, yypvt[-1].y_node, yypvt[-0].y_node);
} break;
case 35:
# line 264 "commands.y"
{
	yyval.y_node = build(yypvt[-2].y_op, yypvt[-1].y_node, nil, yypvt[-0].y_node);
} break;
case 36:
# line 269 "commands.y"
{
	yyval.y_node = build(yypvt[-1].y_op, nil, nil, yypvt[-0].y_node);
} break;
case 37:
# line 274 "commands.y"
{
	yyval.y_node = build(O_WHATIS, yypvt[-0].y_node);
} break;
case 38:
# line 279 "commands.y"
{
	yyval.y_node = build(O_ADDEVENT, yypvt[-3].y_node, yypvt[-1].y_cmdlist);
} break;
case 39:
# line 284 "commands.y"
{
	yyval.y_node = build(O_WHEREIS, yypvt[-0].y_node);
} break;
case 40:
# line 289 "commands.y"
{
	yyval.y_node = build(O_WHICH, yypvt[-0].y_node);
} break;
case 41:
# line 294 "commands.y"
{
	String dir;

	yyval.y_node = nil;
	if (list_size(yypvt[-0].y_list) == 0) {
	    foreach (String, dir, sourcepath)
		printf("%s ", dir);
	    endfor
	    printf("\n");
	} else {
	    foreach (String, dir, sourcepath)
		list_delete(list_curitem(sourcepath), sourcepath);
	    endfor
	    sourcepath = yypvt[-0].y_list;
	}
} break;
case 42:
# line 312 "commands.y"
{ arginit(); } break;
case 45:
# line 318 "commands.y"
{
	fflush(stdout);
} break;
case 48:
# line 329 "commands.y"
{
	newarg(ident(yypvt[-0].y_name));
} break;
case 49:
# line 334 "commands.y"
{
	inarg(ident(yypvt[-0].y_name));
} break;
case 50:
# line 339 "commands.y"
{
	outarg(ident(yypvt[-0].y_name));
} break;
case 51:
# line 345 "commands.y"
{
	yyval.y_node = build(O_STEP, true, false);
} break;
case 52:
# line 350 "commands.y"
{
	yyval.y_node = build(O_STEP, false, false);
} break;
case 53:
# line 355 "commands.y"
{
	yyval.y_node = build(O_STEP, true, true);
} break;
case 54:
# line 360 "commands.y"
{
	yyval.y_node = build(O_STEP, false, true);
} break;
case 55:
# line 366 "commands.y"
{
	beginshellmode();
} break;
case 56:
# line 372 "commands.y"
{
	yyval.y_list = yypvt[-1].y_list;
	list_append(list_item(ident(yypvt[-0].y_name)), nil, yyval.y_list);
} break;
case 57:
# line 378 "commands.y"
{
	yyval.y_list = list_alloc();
} break;
case 60:
# line 389 "commands.y"
{
	yyval.y_cmdlist = yypvt[-2].y_cmdlist;
	cmdlist_append(yypvt[-1].y_node, yyval.y_cmdlist);
} break;
case 61:
# line 395 "commands.y"
{
	yyval.y_cmdlist = list_alloc();
	cmdlist_append(yypvt[-1].y_node, yyval.y_cmdlist);
} break;
case 64:
# line 411 "commands.y"
{
	yyval.y_node = build(O_PRINT, yypvt[-0].y_node);
} break;
case 65:
# line 416 "commands.y"
{
	yyval.y_node = build(O_WHERE);
} break;
case 66:
# line 421 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 67:
# line 426 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 68:
# line 431 "commands.y"
{
 	yyval.y_node = build(O_DEBUG, yypvt[-0].y_long);
} break;
case 69:
# line 436 "commands.y"
{
	yyval.y_node = build(O_DUMP);
} break;
case 70:
# line 441 "commands.y"
{
	yyval.y_node = build(O_STATUS);
} break;
case 71:
# line 447 "commands.y"
{
	yyval.y_node = build(O_ALIAS, build(O_NAME, yypvt[-1].y_name), build(O_NAME, yypvt[-0].y_name));
} break;
case 72:
# line 452 "commands.y"
{
	yyval.y_node = build(O_ALIAS, build(O_NAME, yypvt[-0].y_name), nil);
} break;
case 73:
# line 457 "commands.y"
{
	yyval.y_node = build(O_ALIAS, nil, nil);
} break;
case 74:
# line 463 "commands.y"
{
	yyval.y_op = O_TRACE;
} break;
case 75:
# line 468 "commands.y"
{
	yyval.y_op = O_TRACEI;
} break;
case 76:
# line 474 "commands.y"
{
	yyval.y_op = O_STOP;
} break;
case 77:
# line 479 "commands.y"
{
	yyval.y_op = O_STOPI;
} break;
case 78:
# line 485 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 79:
# line 490 "commands.y"
{
	yyval.y_node = build(O_QLINE, build(O_SCON, yypvt[-2].y_string), yypvt[-0].y_node);
} break;
case 80:
# line 496 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 81:
# line 501 "commands.y"
{
	yyval.y_node = build(O_QLINE, build(O_SCON, cursource), yypvt[-0].y_node);
} break;
case 82:
# line 506 "commands.y"
{
	yyval.y_node = build(O_QLINE, build(O_SCON, yypvt[-2].y_string), yypvt[-0].y_node);
} break;
case 83:
# line 512 "commands.y"
{
	yyval.y_string = ident(yypvt[-0].y_name);
} break;
case 84:
# line 518 "commands.y"
{
	yyval.y_string = nil;
} break;
case 85:
# line 523 "commands.y"
{
	yyval.y_string = yypvt[-0].y_string;
} break;
case 86:
# line 529 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 87:
# line 534 "commands.y"
{
	yyval.y_node = nil;
} break;
case 88:
# line 540 "commands.y"
{
	yyval.y_node = build(O_LIST,
	    build(O_LCON, (long) cursrcline),
	    build(O_LCON, (long) cursrcline + 9)
	);
} break;
case 89:
# line 548 "commands.y"
{
	yyval.y_node = build(O_LIST, yypvt[-0].y_node, yypvt[-0].y_node);
} break;
case 90:
# line 553 "commands.y"
{
	yyval.y_node = build(O_LIST, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 91:
# line 558 "commands.y"
{
	yyval.y_node = build(O_LIST, yypvt[-0].y_node);
} break;
case 92:
# line 564 "commands.y"
{
	yyval.y_node = build(O_LCON, yypvt[-0].y_long);
} break;
case 93:
# line 569 "commands.y"
{
	yyval.y_node = build(O_LCON, (long) LASTLINE);
} break;
case 94:
# line 575 "commands.y"
{
	yyval.y_node = build(O_EXAMINE, yypvt[-0].y_string, yypvt[-3].y_node, nil, yypvt[-1].y_long);
} break;
case 95:
# line 580 "commands.y"
{
	yyval.y_node = build(O_EXAMINE, yypvt[-0].y_string, yypvt[-4].y_node, yypvt[-2].y_node, 0);
} break;
case 96:
# line 585 "commands.y"
{
	yyval.y_node = build(O_EXAMINE, yypvt[-0].y_string, build(O_LCON, (long) prtaddr), nil, yypvt[-1].y_long);
} break;
case 97:
# line 590 "commands.y"
{
	yyval.y_node = build(O_EXAMINE, yypvt[-0].y_string, yypvt[-2].y_node, nil, 0);
} break;
case 98:
# line 596 "commands.y"
{
	yyval.y_node = build(O_LCON, yypvt[-0].y_long);
} break;
case 99:
# line 601 "commands.y"
{
	yyval.y_node = amper(yypvt[-0].y_node);
} break;
case 100:
# line 606 "commands.y"
{
	yyval.y_node = build(O_ADD, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 101:
# line 611 "commands.y"
{
	yyval.y_node = build(O_SUB, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 102:
# line 616 "commands.y"
{
	yyval.y_node = build(O_MUL, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 103:
# line 621 "commands.y"
{
	yyval.y_node = build(O_INDIR, yypvt[-0].y_node);
} break;
case 104:
# line 626 "commands.y"
{
	yyval.y_node = yypvt[-1].y_node;
} break;
case 105:
# line 632 "commands.y"
{
	yyval.y_long = 1;
} break;
case 106:
# line 637 "commands.y"
{
	yyval.y_long = yypvt[-0].y_long;
} break;
case 107:
# line 643 "commands.y"
{
	yyval.y_string = ident(yypvt[-0].y_name);
	curformat = yyval.y_string;
} break;
case 108:
# line 649 "commands.y"
{
	yyval.y_string = curformat;
} break;
case 109:
# line 655 "commands.y"
{
	yyval.y_node = nil;
} break;
case 110:
# line 660 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 111:
# line 666 "commands.y"
{
	yyval.y_node = build(O_COMMA, yypvt[-0].y_node, nil);
} break;
case 112:
# line 671 "commands.y"
{
	yyval.y_node = build(O_COMMA, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 113:
# line 677 "commands.y"
{
	yyval.y_node = build(O_RVAL, yypvt[-0].y_node);
} break;
case 114:
# line 682 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 115:
# line 687 "commands.y"
{
	yyval.y_node = build(O_TYPERENAME, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 116:
# line 692 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 117:
# line 697 "commands.y"
{
	yyval.y_node = build(O_NEG, yypvt[-0].y_node);
} break;
case 118:
# line 702 "commands.y"
{
	yyval.y_node = amper(yypvt[-0].y_node);
} break;
case 119:
# line 707 "commands.y"
{
	yyval.y_node = build(O_ADD, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 120:
# line 712 "commands.y"
{
	yyval.y_node = build(O_SUB, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 121:
# line 717 "commands.y"
{
	yyval.y_node = build(O_MUL, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 122:
# line 722 "commands.y"
{
	yyval.y_node = build(O_DIVF, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 123:
# line 727 "commands.y"
{
	yyval.y_node = build(O_DIV, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 124:
# line 732 "commands.y"
{
	yyval.y_node = build(O_MOD, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 125:
# line 737 "commands.y"
{
	yyval.y_node = build(O_AND, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 126:
# line 742 "commands.y"
{
	yyval.y_node = build(O_OR, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 127:
# line 747 "commands.y"
{
	yyval.y_node = build(O_LT, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 128:
# line 752 "commands.y"
{
	yyval.y_node = build(O_LE, yypvt[-3].y_node, yypvt[-0].y_node);
} break;
case 129:
# line 757 "commands.y"
{
	yyval.y_node = build(O_GT, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 130:
# line 762 "commands.y"
{
	yyval.y_node = build(O_GE, yypvt[-3].y_node, yypvt[-0].y_node);
} break;
case 131:
# line 767 "commands.y"
{
	yyval.y_node = build(O_EQ, yypvt[-2].y_node, yypvt[-0].y_node);
} break;
case 132:
# line 772 "commands.y"
{
	yyval.y_node = build(O_EQ, yypvt[-3].y_node, yypvt[-0].y_node);
} break;
case 133:
# line 777 "commands.y"
{
	yyval.y_node = build(O_NE, yypvt[-3].y_node, yypvt[-0].y_node);
} break;
case 134:
# line 782 "commands.y"
{
	yyval.y_node = build(O_NE, yypvt[-3].y_node, yypvt[-0].y_node);
} break;
case 135:
# line 787 "commands.y"
{
	yyval.y_node = yypvt[-1].y_node;
} break;
case 136:
# line 793 "commands.y"
{
	yyval.y_node = yypvt[-0].y_node;
} break;
case 137:
# line 798 "commands.y"
{
	yyval.y_node = subscript(yypvt[-3].y_node, yypvt[-1].y_node);
} break;
case 138:
# line 803 "commands.y"
{
	yyval.y_node = dot(yypvt[-2].y_node, yypvt[-0].y_name);
} break;
case 139:
# line 808 "commands.y"
{
	yyval.y_node = dot(yypvt[-2].y_node, yypvt[-0].y_name);
} break;
case 140:
# line 813 "commands.y"
{
	yyval.y_node = build(O_INDIR, yypvt[-0].y_node);
} break;
case 141:
# line 818 "commands.y"
{
	yyval.y_node = build(O_INDIR, yypvt[-1].y_node);
} break;
case 142:
# line 823 "commands.y"
{
	yyval.y_node = build(O_INDIR, yypvt[-1].y_node);
} break;
case 143:
# line 828 "commands.y"
{
	yyval.y_node = concrete(yypvt[-0].y_node);
} break;
case 144:
# line 833 "commands.y"
{
	yyval.y_node = build(O_CALL, yypvt[-3].y_node, yypvt[-1].y_node);
} break;
case 145:
# line 839 "commands.y"
{
	chkboolean(yypvt[-0].y_node);
	yyval.y_node = yypvt[-0].y_node;
} break;
case 146:
# line 846 "commands.y"
{
	yyval.y_node = build(O_LCON, yypvt[-0].y_long);
} break;
case 147:
# line 851 "commands.y"
{
	yyval.y_node = build(O_FCON, yypvt[-0].y_real);
} break;
case 148:
# line 856 "commands.y"
{
	yyval.y_node = build(O_SCON, yypvt[-0].y_string);
} break;
case 149:
# line 862 "commands.y"
{
	yyval.y_node = build(O_SYM, which(yypvt[-0].y_name));
} break;
case 150:
# line 868 "commands.y"
{
	yyval.y_name = yypvt[-0].y_name;
} break;
case 151:
# line 873 "commands.y"
{
	yyval.y_name = yypvt[-0].y_name;
} break;
		}
		goto yystack;  /* stack new state and value */

	}
