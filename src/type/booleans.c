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

#include "booleans.h"
#include "base.h"

static size_t booleantype = 0;

/* just get an non NULL value, "(void *) 1" is impl-dependent */
#define ESCM_BOOL_T ((void *) &booleantype)
#define ESCM_BOOL_F NULL

static void boolean_print(escm *, void *, escm_output *, int);
static int boolean_equal(escm *, void *, void *, int);
static int boolean_parsetest(escm *, escm_input *, tint);
static escm_atom *boolean_parse(escm *, escm_input *);

void
escm_booleans_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->print.fprint = (Escm_Fun_Print) boolean_print;
    t->equal.fequal = (Escm_Fun_Equal) boolean_equal;
    t->parsetest.fparsetest = boolean_parsetest;
    t->parse.fparse = boolean_parse;

    booleantype = escm_type_add(e, t);

    e->TRUE = escm_atom_new(e, ESCM_TYPE_BOOLEAN, ESCM_BOOL_T);
    e->FALSE = escm_atom_new(e, ESCM_TYPE_BOOLEAN, ESCM_BOOL_F);

    (void) escm_procedure_new(e, T("not"), 1, 1, escm_not, NULL);
    (void) escm_procedure_new(e, T("boolean?"), 1, 1, escm_boolean_p, NULL);
}

escm_atom *
escm_not(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return !ESCM_ISTRUE(e, escm_cons_car(args)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_boolean_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return ESCM_ISBOOL(escm_cons_car(args)) ? e->TRUE : e->FALSE;
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

    escm_printf(stream, T("#%lc"), (bool == NULL) ? T('f') : T('t'));
}

static int
boolean_equal(escm *e, void *b1, void *b2, int lvl)
{
    (void) e;
    (void) lvl;

    return (b1 == b2);
}

static int
boolean_parsetest(escm *e, escm_input *stream, tint c)
{
    if (c == T('#')) {
        tint c2;

        c2 = escm_input_peek(stream);
        if (e->casesensitive == 0)
            c2 = tolower(c2);
        return (c2 == T('t') || c2 == T('f'));
    }

    return 0;
}

static escm_atom *
boolean_parse(escm *e, escm_input *stream)
{
    tint c;

    (void) escm_input_getc(stream); /* skip '#' */

    c = escm_input_getc(stream);
    if (e->casesensitive == 0)
        c = tolower(c);
    return (c == T('t')) ? e->TRUE : e->FALSE;
}
