#ifndef languages_h
#define languages_h
typedef struct Language *Language;

typedef enum {
    L_PRINTDECL, L_PRINTVAL, L_TYPEMATCH, L_BUILDAREF, L_EVALAREF
} LanguageOp;

typedef LanguageOperation();
language_init(/*  */);
Language findlanguage(/* suffix */);
String language_name(/* lang */);
Language language_define(/* name, suffix */);
language_setop(/* lang, op, operation */);
LanguageOperation *language_op(/* lang, op */);
#endif
