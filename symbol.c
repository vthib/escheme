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

#include "symbol.h"
#include "escheme.h"

static size_t symboltype = 0;

static void symbol_print(escm *, char *, FILE *);
static int symbol_equal(escm *, char *, char *, unsigned int);
static int symbol_parsetest(escm *, int);
static escm_atom *symbol_parse(escm *);
static escm_atom *symbol_eval(escm *, const char *);

static inline int issymbol(int);

void
escm_symbol_init(escm *e)
{
    escm_type *t;

    t = xmalloc(sizeof *t);
    t->fmark = NULL;
    t->ffree = (Escm_Fun_Free) free;
    t->fprint = (Escm_Fun_Print) symbol_print;
    t->fequal = (Escm_Fun_Equal) symbol_equal;
    t->fparsetest = symbol_parsetest;
    t->fparse = symbol_parse;
    t->feval = (Escm_Fun_Eval) symbol_eval;

    symboltype = escm_type_add(e, t);
}

size_t
escm_symbol_tget(void)
{
    return symboltype;
}

static void
symbol_print(escm *e, char *symbol, FILE *stream)
{
    (void) e;

    fprintf(stream, "%s", symbol);
}

static int
symbol_equal(escm *e, char *s1, char *s2, unsigned int lvl)
{
    (void) e;
    (void) lvl;

    return (s1 == s2 || !strcmp(s1, s2));
}

static int
symbol_parsetest(escm *e, int c)
{
    (void) e;

    return issymbol(c);
}    

static escm_atom *
symbol_parse(escm *e)
{
    char *str;

    str = escm_input_getstr_fun(e->input, issymbol);

    return escm_atom_new(e, symboltype, str);
}

static escm_atom *
symbol_eval(escm *e, const char *sym)
{
    escm_atom *atom;

    atom = escm_env_get(e->env, sym);
    if (!atom) {
	escm_input_print(e->input, "unknown symbol `%s'.", sym);
	e->err = -1;
	return NULL;
    }

    return atom;
}

static inline int
issymbol(int c)
{
    return (strchr("!$%&*+-./:<=>?@^_~", c) != NULL || isalnum(c));
}

