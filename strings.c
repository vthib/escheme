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
#include <string.h>
#include <ctype.h>

#include "strings.h"
#include "escheme.h"

static size_t stringtype = 0;

static void string_print(escm *, char *, FILE *);
static int string_equal(escm *, char *, char *, unsigned int);
static int string_parsetest(escm *, int);
static escm_atom *string_parse(escm *);

void
escm_string_init(escm *e)
{
    escm_type *t;

    t = xmalloc(sizeof *t);
    t->fmark = NULL;
    t->ffree = (Escm_Fun_Free) free;
    t->fprint = (Escm_Fun_Print) string_print;
    t->fequal = (Escm_Fun_Equal) string_equal;
    t->fparsetest = string_parsetest;
    t->fparse = string_parse;
    t->feval = NULL;

    stringtype = escm_type_add(e, t);
}

size_t
escm_string_tget(void)
{
    return stringtype;
}

static void
string_print(escm *e, char *string, FILE *stream)
{
    size_t n;

    (void) e;

    n = mbstowcs(NULL, string, 0) + 1;
    if (n == 0)
	fprintf(stream, "\"%s\"", string);
    else {
	wchar_t *wc;
	wc = xcalloc(n, sizeof *wc);
	(void) mbstowcs(wc, string, n); /* check -1 ? */

	fprintf(stream, "\"%ls\"", wc);

	free(wc);
    }

    fprintf(stream, "-> %ld", (n != 0) ? n - 1 : strlen(string));
}

static int
string_equal(escm *e, char *s1, char *s2, unsigned int lvl)
{
    (void) e;

    switch (lvl) {
    case 0: case 1: /* eq? & eqv?: true if same pointer */
	return s1 == s2;
    case 2: default: /* equal? */
	return !strcmp(s1, s2);
    }
}

static int
string_parsetest(escm *e, int c)
{
    (void) e;

    return c == '"';
}    

static escm_atom *
string_parse(escm *e)
{
    char *str;

    (void) escm_input_getc(e->input); /* skip '"' */
    str = escm_input_gettext(e->input, "\"");
    (void) escm_input_getc(e->input); /* skip '"' */

    return escm_atom_new(e, stringtype, str);
}
