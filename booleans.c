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

#include "escheme.h"

#define ESCM_BOOL_T ((void *) 1)
#define ESCM_BOOL_F NULL

static size_t booleantype = 0;

static void boolean_print(escm *, void *, escm_output *, int);
static int boolean_equal(escm *, void *, void *, int);
static int boolean_parsetest(escm *, int);
static escm_atom *boolean_parse(escm *);

void
escm_booleans_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fprint = (Escm_Fun_Print) boolean_print;
    t->fequal = (Escm_Fun_Equal) boolean_equal;
    t->fparsetest = boolean_parsetest;
    t->fparse = boolean_parse;

    booleantype = escm_type_add(e, t);

    e->TRUE = escm_atom_new(e, ESCM_TYPE_BOOL, ESCM_BOOL_T);
    e->FALSE = escm_atom_new(e, ESCM_TYPE_BOOL, ESCM_BOOL_F);

    (void) escm_procedure_new(e, "not", 1, 1, escm_not, NULL);
    (void) escm_procedure_new(e, "boolean?", 1, 1, escm_boolean_p, NULL);
}

escm_atom *
escm_not(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);

    if (!ESCM_ISTRUE(a))
	return e->TRUE;
    else
	return e->FALSE;
}

escm_atom *
escm_boolean_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);

    return ESCM_ISBOOL(a) ? e->TRUE : e->FALSE;
}

size_t
escm_boolean_tget(void)
{
    return booleantype;
}

static void
boolean_print(escm *e, void *bool, escm_output *stream, int lvl)
{
    (void) e;
    (void) lvl;

    escm_printf(stream, "#%c", (bool == ESCM_BOOL_T) ? 't' : 'f');
}

static int
boolean_equal(escm *e, void *b1, void *b2, int lvl)
{
    (void) e;
    (void) lvl;

    return (b1 == b2);
}

static int
boolean_parsetest(escm *e, int c)
{
    if (c == '#') {
	int c2;
	int ret;

	c2 = escm_input_getc(e->input);
	ret = (c2 == 't' || c2 == 'f');
	escm_input_ungetc(e->input, c2);

	return ret;
    }

    return 0;
}    

static escm_atom *
boolean_parse(escm *e)
{
    (void) escm_input_getc(e->input); /* skip '#' */

    return escm_atom_new(e, booleantype, (escm_input_getc(e->input) == 't') ?
			ESCM_BOOL_T : ESCM_BOOL_F);
}
