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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <time.h>

#include "escheme.h"

enum {
#ifdef ESCM_USE_NUMBERS
    NUM,
#endif
#ifdef ESCM_USE_BOOLEANS
    BOOL,
#endif
#ifdef ESCM_USE_STRINGS
    STRING,
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
#ifdef ESCM_USE_RECORDS
    RECORD,
#endif
#ifdef ESCM_USE_DYNTYPES
    DYNTYPE,
#endif
    MAXTYPE
};

struct types {
    char *name;
    char *desc;
    void (*funinit)(escm *);
    int load;
};

static struct types desc[MAXTYPE] = {
#ifdef ESCM_USE_NUMBERS
    { "numbers", "numbers: the basic number implementation.",
      escm_numbers_init, 1 },
#endif
#ifdef ESCM_USE_BOOLEANS
    { "booleans", "booleans: the boolean implementation.",
      escm_booleans_init, 1 },
#endif
#ifdef ESCM_USE_STRINGS
    { "strings", "strings: the string implementation.",
      escm_strings_init, 1 },
#endif
#ifdef ESCM_USE_VECTORS
    { "vectors", "vectors: the vector implementation.",
      escm_vectors_init, 1 },
#endif
#ifdef ESCM_USE_CHARACTERS
    { "characters", "characters: the character implementation.",
      escm_chars_init, 1 },
#endif
#ifdef ESCM_USE_PROMISES
    { "promises", "promises: the promises implementation (delay and force).",
      escm_promises_init, 1 },
#endif
#ifdef ESCM_USE_PORTS
    { "ports", "ports: the port implementation.", escm_ports_init, 1 },
#endif
#ifdef ESCM_USE_MACROS
    { "macros", "macros: the macro implementation (scheme hygienic macros).",
      escm_macros_init, 1 },
#endif
#ifdef ESCM_USE_CONTINUATIONS
    { "continuations", "continuations: the continuation implementation.",
      escm_continuations_init, 1 },
#endif
#ifdef ESCM_USE_RECORDS
    { "records", "records: the record implementation.",
      escm_records_init, 1 },
#endif
#ifdef ESCM_USE_DYNTYPES
    { "dyntypes", "dyntypes: the dynamic types implementation (escheme "
      "system to create or \n\t\tmodify types impl at runtime).",
      escm_dyntypes_init, 1 }
#endif
};

static void usage(char *);

int
main(int argc, char **argv)
{
    escm *e;
    int i, j;
    int casesens = 1;
    int resume = 0;
    char *p;
    char *evalstr;
    int ret;

    if (!setlocale(LC_ALL, "") ||
        !setlocale(LC_NUMERIC, "C"))  /* corrects strtod interpretation */
        ftprintf(stderr, T("can't set the locale.\n"));

    evalstr = NULL;

    srand(time(NULL));

    for (i = 1; i < argc; i++) {
        if (*argv[i] == '-') {
            if (argv[i][1] == '-') {
                if (0 == strcmp(argv[i], "--noload")) {
                    char *comma;

                    if (i+1 == argc) {
                        ftprintf(stderr,
                                 _(T("missing arguments to --noload.\n")));
                        break;
                    }
                    p = argv[i+1], ret = 1;
                    while (ret) {
                        comma = strchr(p, ',');
                        if (!comma)
                            comma = p + strlen(p), ret = 0;
                        else
                            *comma = '\0';

                        for (j = 0; j < MAXTYPE; j++) {
                            if (0 == strcmp(p, desc[j].name)) {
                                desc[j].load = 0;
                                break;
                            }
                        }

                        if (j == MAXTYPE)
                            ftprintf(stderr, _(T("unknown argument to "))
                                    _(T("--noload:%s.\n")), p);

                        p = comma + 1;
                    }
                    i++;
                } else
                    ftprintf(stderr, _(T("unknown option %s.\n")), argv[i]);
            } else {
                for (p = argv[i] + 1; *p != '\0'; p++) {
                    switch (*p) {
                    case 'g': casesens = 1; break;
                    case 'G': casesens = 0; break;
                    case 'r': resume = 1; break;
                    case 'e': evalstr = argv[++i]; break;
                    case 'h':
                        usage(argv[0]);
                        return EXIT_SUCCESS;
                    default:
                        ftprintf(stderr, _(T("unknown option -%c.\n")), *p);
                    }
                }
            }
        } else
            break;
    }

    e = escm_new();
    if (!e)
        return EXIT_FAILURE;

    for (j = 0; j < MAXTYPE; j++) {
        if (desc[j].load)
            desc[j].funinit(e);
    }

    e->casesensitive = casesens;
    e->backtrace = 1;

    escm_init(e);

    ret = 1;
    if (evalstr) {
        tchar *tcs;

        tcs = strtotcs(evalstr);
        ret = escm_sparse(e, tcs);
        free(tcs);
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
    int i;

    printf("Escheme -- A small and smart scheme interpreter.\n"
           "\n"
           "%s [option ...] [file ...]\n"
           "if no file is present, display a prompt.\n"
           "\n"
           "-h\tprint this help.\n"
           "-G\tSymbols are treated case-insensitively.\n"
           "-g\tSymbols are treated case-sensitively (default).\n"
           "-e expr\tEvaluates expr.\n\n"
           "\n"
           "--noload type1[,type2[,...]]\n"
           "    Do not load the given types. The following types are "
           "recognized :\n", name);

    for (i = 0; i < MAXTYPE; i++)
        printf("\t%s\n", desc[i].desc);
}
