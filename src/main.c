/* 
 * Copyright (c) 2007 Vincent "drexil" Thiberville <mahnmut@gmail.com>
 *
 * This file is part of Escheme. Escheme is free software; you can redistribute
 * it and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 * Escheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Escheme; If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdlib.h>
#include <string.h>
#include <locale.h>

#include "escheme.h"

enum {
#ifdef ESCM_USE_CNUMBERS
    CNUM,
#endif
#ifdef ESCM_USE_BNUMBERS
    BNUM,
#endif
#ifdef ESCM_USE_STRINGS
    STRING,
#endif
#ifdef ESCM_USE_BOOLEANS
    BOOL,
#endif
#ifdef ESCM_USE_VECTORS
    VECT,
#endif
#ifdef ESCM_USE_CHARACTERS
    CHAR,
#endif
#ifdef ESCM_USE_PROMISES
    PROMISE,
#endif
#ifdef ESCM_USE_PORTS
    PORT,
#endif
#ifdef ESCM_USE_MACROS
    MACRO,
#endif
#ifdef ESCM_USE_CONTINUATIONS
    CONT,
#endif
#ifdef ESCM_USE_DYNTYPES
    DYNTYPE,
#endif
    MAXTYPE
};

static void usage(char *);

int
main(int argc, char **argv)
{
    escm *e;
    int i;
    int noload[MAXTYPE];
    int casesens = 1;
    int useascii = 0;
    int resume = 0;
    int loadinit = 1;
    char *p;
    char *evalstr;
    int ret;

    memset(noload, 0, sizeof noload);

    if (!setlocale(LC_ALL, "") ||
        !setlocale(LC_NUMERIC, "C"))  /* corrects strtod interpretation */
        fprintf(stderr, "can't set the locale.\n");

    evalstr = NULL;

    for (i = 1; i < argc; i++) {
        if (*argv[i] == '-') {
            if (argv[i][1] == '-') {
                if (0 == strcmp(argv[i], "--noload")) {
                    char *comma;

                    if (i+1 == argc) {
                        fprintf(stderr, "missing arguments to --noload.\n");
                        break;
                    }
                    p = argv[i+1], ret = 1;
                    while (ret) {
                        comma = strchr(p, ',');
                        if (!comma)
                            comma = p + strlen(p), ret = 0;
                        else
                            *comma = '\0';

#ifdef ESCM_USE_CNUMBERS
                        if (0 == strcmp(p, "cnumbers")) {
                            noload[CNUM] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_BNUMBERS
                        if (0 == strcmp(p, "bnumbers")) {
                            noload[BNUM] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_NUMBERS
                        if (0 == strcmp(p, "numbers")) {
# ifdef ESCM_USE_BNUMBERS
                            noload[BNUM] = 1;
# endif
# ifdef ESCM_USE_CNUMBERS
                            noload[CNUM] = 1;
# endif
                            goto loop;
                        }
#endif /* ESCM_USE_NUMBERS */
#ifdef ESCM_USE_STRINGS
                        if (0 == strcmp(p, "strings")) {
                            noload[STRING] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_BOOLEANS
                        if (0 == strcmp(p, "booleans")) {
                            noload[BOOL] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_VECTORS
                        if (0 == strcmp(p, "vectors")) {
                            noload[VECT] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_CHARACTERS
                        if (0 == strcmp(p, "characters")) {
                            noload[CHAR] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_PROMISES
                        if (0 == strcmp(p, "promises")) {
                            noload[PROMISE] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_PORTS
                        if (0 == strcmp(p, "ports")) {
                            noload[PORT] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_MACROS
                        if (0 == strcmp(p, "macros")) {
                            noload[PORT] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_CONTINUATIONS
                        if (0 == strcmp(p, "continuations")) {
                            noload[CONT] = 1; goto loop;
                        }
#endif
#ifdef ESCM_USE_DYNTYPES
                        if (0 == strcmp(p, "dyntypes")) {
                            noload[DYNTYPE] = 1; goto loop;
                        }
#endif

                        fprintf(stderr, "unknown argument to --noload: %s.\n",
                                p);

                    loop:
                        p = comma + 1;
                    }
                    i++;
                } else
                    fprintf(stderr, "unknown option %s.\n", argv[i]);
            } else {
                for (p = argv[i] + 1; *p != '\0'; p++) {
                    switch (*p) {
                    case 'a': useascii = 1; break;
                    case 'g': casesens = 1; break;
                    case 'G': casesens = 0; break;
                    case 'r': resume = 1; break;
                    case 'S': loadinit = 0; break;
                    case 'e': evalstr = argv[++i]; break;
                    case 'h':
                        usage(argv[0]);
                        return EXIT_SUCCESS;
                    default:
                        fprintf(stderr, "unknown option -%c.\n", *p);
                    }
                }
            }
        } else
            break;
    }

    e = escm_new();
    if (!e)
        return EXIT_FAILURE;

#ifdef ESCM_USE_BOOLEANS
    if (!noload[BOOL])
        escm_booleans_init(e);
#endif

/* numbers needs to be declared before symbols */
#ifdef ESCM_USE_BNUMBERS
    if (!noload[BNUM])
        escm_bnumbers_init(e);
#endif

#ifdef ESCM_USE_CNUMBERS
    if (!noload[CNUM] && noload[BNUM])
        escm_cnumbers_init(e);
#endif

#ifdef ESCM_USE_STRINGS
    if (!noload[STRING]) {
# ifdef ESCM_USE_UNICODE
        if (!useascii)
            escm_ustrings_init(e);
        else
# endif
            escm_astrings_init(e);
    }
#endif
#ifdef ESCM_USE_VECTORS
    if (!noload[VECT])
        escm_vectors_init(e);
#endif
#ifdef ESCM_USE_CHARACTERS
    if (!noload[CHAR]) {
# ifdef ESCM_USE_UNICODE
        if (!useascii)
            escm_uchars_init(e);
        else
# endif
            escm_achars_init(e);
    }
#endif

#ifdef ESCM_USE_PROMISES
    if (!noload[PROMISE])
        escm_promises_init(e);
#endif
#ifdef ESCM_USE_PORTS
    if (!noload[PORT])
        escm_ports_init(e);
#endif
#ifdef ESCM_USE_MACROS
    if (!noload[MACRO])
        escm_macros_init(e);
#endif
#ifdef ESCM_USE_CONTINUATIONS
    if (!noload[CONT])
        escm_continuations_init(e);
#endif

#ifdef ESCM_USE_DYNTYPES
    if (!noload[DYNTYPE])
        escm_dyntypes_init(e);
#endif

#ifndef ESCM_USE_CHARACTERS
    e->EOF_OBJ = e->FALSE;
#endif

    e->casesensitive = casesens;
    e->backtrace = 1;

    escm_init(e, loadinit);

    ret = 1;
    if (evalstr) {
        ret = escm_sparse(e, evalstr);
        if (ret == 0)
            goto end;
        if (i >= argc) {
            if (resume)
                escm_shell(e);
            else
                goto end;
        }
    }

    if (i < argc) {
        while (i < argc) {
            ret = escm_fparse(e, argv[i++]);
            if (ret == 0)
                break;
        }
        if (resume)
            escm_shell(e);
    } else
        escm_shell(e);

end:
    escm_free(e);
    return (ret) ? EXIT_SUCCESS : EXIT_FAILURE;
}

static void
usage(char *name)
{
    printf("Escheme -- A small and smart scheme interpreter.\n"
           "\n"
           "%s [option ...] [file ...]\n"
           "if no file is present, display a prompt.\n"
           "\n"
           "-h\tprint this help.\n"
           "-a\tuse ascii (do not use unicode version of char and string).\n"
           "-G\tSymbols are treated case-insensitively.\n"
           "-g\tSymbols are treated case-sensitively (default).\n"
           "-e expr\tEvaluates expr.\n\n"
           "\n"
           "--noload type1[,type2[,...]]\n"
           "    Do not load the given types. The following types are "
           "recognized :\n", name);

    printf(""
#ifdef ESCM_USE_CNUMBERS
           "\tcnumbers: the complete number implementation.\n"
#endif
#ifdef ESCM_USE_BNUMBERS
           "\tbnumbers: the basic number implementation.\n"
#endif
#ifdef ESCM_USE_NUMBERS
           "\tnumbers: all numbers implementations.\n"
#endif
#ifdef ESCM_USE_STRINGS
           "\tstrings: the string implementation.\n"
#endif
#ifdef ESCM_USE_BOOLEANS
           "\tbooleans: the boolean implementation.\n"
#endif
#ifdef ESCM_USE_VECTORS
           "\tvectors: the vector implementation.\n"
#endif
#ifdef ESCM_USE_CHARACTERS
           "\tcharacters: the character implementation.\n"
#endif
#ifdef ESCM_USE_PROMISES
           "\tpromises: the promises implementation (delay and force)\n"
#endif
        );
    printf(""
#ifdef ESCM_USE_PORTS
           "\tports: the port implementation.\n"
#endif
#ifdef ESCM_USE_MACROS
           "\tmacros: the macro implementation (scheme hygienic macros).\n"
#endif
#ifdef ESCM_USE_CONTINUATIONS
           "\tcontinuations: the continuation implementation.\n"
#endif
#ifdef ESCM_USE_DYNTYPES
           "\tdyntypes: the dynamic types implementation (escheme system "
           "to create or \n\t\tmodify types impl at runtime).\n"
#endif
        );
}
