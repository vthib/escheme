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
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include "escheme.h"

static size_t symboltype = 0;

static void symbol_print(escm *, char *, FILE *, int);
static int symbol_equal(escm *, char *, char *, int);
static int symbol_parsetest(escm *, int);
static escm_atom *symbol_parse(escm *);
static escm_atom *symbol_eval(escm *, const char *);

static inline int issymbol(int);

void
escm_symbols_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) free;
    t->fprint = (Escm_Fun_Print) symbol_print;
    t->fequal = (Escm_Fun_Equal) symbol_equal;
    t->fparsetest = symbol_parsetest;
    t->fparse = symbol_parse;
    t->feval = (Escm_Fun_Eval) symbol_eval;

    symboltype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "symbol?", 1, 1, escm_symbol_p, NULL);
#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, "symbol->string", 1, 1, escm_symbol_to_string,
			      NULL);
    (void) escm_procedure_new(e, "string->symbol", 1, 1, escm_string_to_symbol,
			      NULL);
#endif
}

size_t
escm_symbol_tget(void)
{
    return symboltype;
}

escm_atom *
escm_symbol_make(escm *e, const char *str)
{
    return escm_atom_new(e, symboltype, xstrdup(str));
}

escm_atom *
escm_symbol_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    return ESCM_ISSYM(a) ? e->TRUE : e->FALSE;
}

#ifdef ESCM_USE_STRINGS
escm_atom *
escm_symbol_to_string(escm *e, escm_atom *args)
{
    escm_atom *sym;

    sym = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(sym), sym, e);

    return escm_atom_new(e, ESCM_TYPE_STRING, xstrdup(escm_sym_val(sym)));
}

escm_atom *
escm_string_to_symbol(escm *e, escm_atom *args)
{
    escm_atom *str;

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    return escm_atom_new(e, ESCM_TYPE_SYMBOL, xstrdup(escm_str_val(str)));
}
#endif

static void
symbol_print(escm *e, char *symbol, FILE *stream, int lvl)
{
    (void) e;

    if (lvl == 0) {
	print_slashify(stream, symbol);
	return;
    }

    fprintf(stream, "%s", symbol);
}

static int
symbol_equal(escm *e, char *s1, char *s2, int lvl)
{
    (void) e;
    (void) lvl;

    return (s1 == s2 || !strcmp(s1, s2));
}

static int
symbol_parsetest(escm *e, int c)
{
    (void) e;

    if (isdigit(c))
	return 0;
    if (c == '.') {
	int c2, ret;

	c2 = escm_input_getc(e->input);
	ret = (c2 == '.');
	escm_input_ungetc(e->input, c2);
	return ret;
    }
    if (c == '+' || c == '-') {
	int c2, ret;

	c2 = escm_input_getc(e->input);
	ret = !(isdigit(c2) || c2 == '.');
	escm_input_ungetc(e->input, c2);
	return ret;
    }
    return issymbol(c);
}

static escm_atom *
symbol_parse(escm *e)
{
    char *str;

    str = escm_input_getstr_fun(e->input, issymbol, e->casesensitive);

    return escm_atom_new(e, symboltype, str);
}

static escm_atom *
symbol_eval(escm *e, const char *sym)
{
    escm_atom *atom;

    atom = escm_env_get(e->env, sym);
    if (!atom) {
	fprintf(stderr, "unknown symbol `%s'.\n", sym);
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
