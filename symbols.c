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
#include <assert.h>
#include <string.h>
#include <ctype.h>
#ifdef ESCM_USE_UNICODE
#include <wchar.h>
#endif

#include "escheme.h"

static unsigned long symboltype = 0;

static void symbol_print(escm *, char *, escm_output *, int);
static int symbol_equal(escm *, char *, char *, int);
static int symbol_parsetest(escm *, int);
static escm_atom *symbol_parse(escm *);
static escm_atom *symbol_eval(escm *, const char *);

static inline int issymbol(int);

void
escm_symbols_init(escm *e)
{
    escm_type *t;
    escm_atom *a;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) free;
    t->d.c.fprint = (Escm_Fun_Print) symbol_print;
    t->d.c.fequal = (Escm_Fun_Equal) symbol_equal;
    t->d.c.fparsetest = symbol_parsetest;
    t->d.c.fparse = symbol_parse;
    t->d.c.feval = (Escm_Fun_Eval) symbol_eval;

    symboltype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "symbol?", 1, 1, escm_symbol_p, NULL);

#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, "symbol->string", 1, 1, escm_symbol_to_string,
			      NULL);
    (void) escm_procedure_new(e, "string->symbol", 1, 1, escm_string_to_symbol,
			      NULL);
#endif

    a = escm_procedure_new(e, "lookup", 1, 2, escm_lookup, NULL);
    escm_proc_val(a)->d.c.quoted = 0x1;
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

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", e->curobj);
	escm_abort(e);
    }

    sym = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(sym), sym, e);

#ifdef ESCM_USE_UNICODE
    if (escm_type_ison(ESCM_TYPE_USTRING)) {
	wchar_t *w;
	escm_atom *a;

	w = strtowcs(escm_sym_val(sym));
	a = escm_ustring_make(e, w, wcslen(w));
	free(w);
	return a;
    } else
#endif
	return escm_astring_make(e, xstrdup(escm_sym_val(sym)),
				 strlen(escm_sym_val(sym)));
}

escm_atom *
escm_string_to_symbol(escm *e, escm_atom *args)
{
    escm_atom *str;

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

#ifdef ESCM_USE_UNICODE
    if (escm_type_ison(ESCM_TYPE_USTRING))
	return escm_atom_new(e, ESCM_TYPE_SYMBOL, wcstostr(escm_ustr_val(str)));
    else
	return escm_atom_new(e, ESCM_TYPE_SYMBOL, xstrdup(escm_astr_val(str)));
#else
	return escm_atom_new(e, ESCM_TYPE_SYMBOL, xstrdup(escm_str_val(str)));
#endif
}
#endif

escm_atom *
escm_lookup(escm *e, escm_atom *args)
{
    escm_atom *sym, *ret, *env;

    sym = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(sym), sym, e);

    env = escm_cons_pop(e, &args);
    if (env) {
	escm_assert(ESCM_ISENV(env), env, e);

	ret = escm_env_get(env, escm_sym_val(sym));
    } else
	ret = escm_env_get(e->env, escm_sym_val(sym));

    return (ret != NULL) ? ret : e->FALSE;
}

static void
symbol_print(escm *e, char *symbol, escm_output *stream, int lvl)
{
    (void) e;

    if (lvl == 0) {
	escm_print_slashify(stream, symbol);
	return;
    }

    escm_printf(stream, "%s", symbol);
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
	escm_abort(e);
    }

    return atom;
}

static inline int
issymbol(int c)
{
    return (strchr("!$%&*+-./:<=>?@^_~", c) != NULL || isalnum(c));
}
