
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
extern YYSTYPE yylval;
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
