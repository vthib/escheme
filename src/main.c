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
    CNUM,
    BNUM,
    STRING,
    BOOL,
    VECT,
    CHAR,
    PROMISE,
    PORT,
    MACRO,
    CONT,
    DYNTYPE,
    MAXTYPE
};

static void usage(char *);

int
main(int argc, char **argv)
{
    escm *e;
    int i;
    unsigned int noload[MAXTYPE];
    unsigned int casesens = 1;
    unsigned int useascii = 0;
    char *p;

    memset(noload, 0, sizeof noload);

    if (!setlocale(LC_ALL, "") ||
        !setlocale(LC_NUMERIC, "C"))  /* corrects strtod interpretation */
        fprintf(stderr, "can't set the locale.\n");

    for (i = 1; i < argc; i++) {
	if (*argv[i] == '-' && argv[i][1] != 'e') {
	    for (p = argv[i] + 1; *p != '\0'; p++) {
		switch (*p) {
		case 'a': useascii = 1; break;
		case 'N': noload[CNUM] = 1; break;
		case 'n': noload[BNUM] = 1; break;
		case 's': noload[STRING] = 1; break;
		case 'b': noload[BOOL] = 1; break;
		case 'v': noload[VECT] = 1; break;
		case 'c': noload[CHAR] = 1; break;
		case 'P': noload[PROMISE] = 1; break;
		case 'p': noload[PORT] = 1; break;
		case 'm': noload[MACRO] = 1; break;
		case 'C': noload[CONT] = 1; break;
		case 'd': noload[DYNTYPE] = 1; break;
		case 'g': casesens = 1; break;
		case 'G': casesens = 0; break;
		case 'h':
		    usage(argv[0]);
		    return EXIT_SUCCESS;
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

    escm_init(e);

    if (i < argc) {
	while (i < argc) {
	    if (0 == strcmp(argv[i], "-e")) {
		(void) escm_sparse(e, argv[i + 1]);
		i += 2;
	    } else
		(void) escm_fparse(e, argv[i++]);
	}
    } else
	escm_shell(e);

    escm_free(e);

    return EXIT_SUCCESS;
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
	   "-N\tdo not load the complete number type.\n"
	   "-n\tdo not load the basic number type.\n"
	   "-s\tdo not load the string type.\n"
	   "-b\tdo not load the boolean type.\n"
	   "-v\tdo not load the vector type.\n"
	   "-c\tdo not load the character type.\n"
	   "-P\tdo not load the promise type.\n"
	   "-p\tdo not load the port type.\n", name);
    printf("-m\tdo not load the macro type.\n"
	   "-C\tdo not load the continuation type.\n"
	   "-d\tdo not load dynamic types primitives.\n"
	   "\n"
	   "-G\tSymbols are treated case-insensitively.\n"
	   "-g\tSymbols are treated case-sensitively (default).\n"
	   "\n"
	   "-e expr\tEvaluates expr.\n\n");
}



