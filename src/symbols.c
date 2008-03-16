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

static unsigned long symboltype = 2;

static void symbol_print(escm *, escm_tst *, escm_output *, int);
static int symbol_parsetest(escm *, int);
static escm_atom *symbol_parse(escm *);
static escm_atom *symbol_eval(escm *, escm_tst *);
static int symbol_equal(escm *, escm_tst *, escm_tst *, int);
static int symbol_eq_colorless(char *, char *);

static inline int issymbol(int);

void
escm_symbols_init(escm *e)
{
    escm_type *t;
    escm_atom *a;

    t = xcalloc(1, sizeof *t);
    t->d.c.fprint = (Escm_Fun_Print) symbol_print;
    t->d.c.fparsetest = symbol_parsetest;
    t->d.c.fparse = symbol_parse;
    t->d.c.feval = (Escm_Fun_Eval) symbol_eval;
    t->d.c.fequal = (Escm_Fun_Equal) symbol_equal;

    symboltype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "symbol?", 1, 1, escm_symbol_p, NULL);

#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, "symbol->string", 1, 1, escm_symbol_to_string,
			      NULL);
    (void) escm_procedure_new(e, "string->symbol", 1, 1, escm_string_to_symbol,
			      NULL);
#endif

    a = escm_procedure_new(e, "lookup", 1, 1, escm_lookup, NULL);
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
    escm_env *env;

    env = e->env->ptr;
    return escm_atom_new(e, symboltype, escm_tst_gettree(&env->tree, str));
}

void
escm_symbol_set(escm_atom *sym, escm_atom *atom)
{
    escm_tst *t;

    t = sym->ptr;
    if (!t->node)
	t->node = xcalloc(1, sizeof *t->node);
    t->node->atom = atom;
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
	escm_error(e, "~s: string type is off.~%", escm_fun(e));
	escm_abort(e);
    }

    sym = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(sym), sym, e);

#ifdef ESCM_USE_UNICODE
    if (escm_type_ison(ESCM_TYPE_USTRING)) {
	wchar_t *w;
	escm_atom *a;

	w = strtowcs(escm_sym_name(sym));
	a = escm_ustring_make(e, w, wcslen(w));
	free(w);
	return a;
    } else
#endif
	return escm_astring_make(e, xstrdup(escm_sym_name(sym)),
				 strlen(escm_sym_name(sym)));
}

escm_atom *
escm_string_to_symbol(escm *e, escm_atom *args)
{
    escm_atom *str;

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", escm_fun(e));
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

#ifdef ESCM_USE_UNICODE
    if (escm_type_ison(ESCM_TYPE_USTRING))
	return escm_symbol_make(e, wcstostr(escm_ustr_val(str)));
    else
	return escm_symbol_make(e, xstrdup(escm_astr_val(str)));
#else
	return escm_symbol_make(e, xstrdup(escm_str_val(str)));
#endif
}
#endif

escm_atom *
escm_lookup(escm *e, escm_atom *args)
{
    escm_atom *sym;

    sym = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(sym), sym, e);

    return (escm_sym_val(sym) != NULL) ? escm_sym_val(sym) : e->FALSE;
}

static void
symbol_print(escm *e, escm_tst *symbol, escm_output *stream, int lvl)
{
    (void) e;

    if (lvl == 0) {
	escm_print_slashify(stream, symbol->symname);
	return;
    }

    escm_printf(stream, "%s", symbol->symname);
}

static int
symbol_parsetest(escm *e, int c)
{
    (void) e;

    if (c == '+' || c == '-') {
	int c2;

	c2 = escm_input_getc(e->input);
	if (c2 == '.') {
	    int ret;

	    ret = !isdigit(escm_input_peek(e->input));
	    escm_input_ungetc(e->input, c2);
	    return ret;
	}
 
	escm_input_ungetc(e->input, c2);
	return !(isdigit(c2) || c2 == 'i');
    } else if (c == '.')
	return !isdigit(escm_input_peek(e->input));
    else if (isdigit(c))
	return 0;
    else
	return issymbol(c);
}

static escm_atom *
symbol_parse(escm *e)
{
    char *str;
    escm_atom *a;

    str = escm_input_getstr_fun(e->input, issymbol, e->casesensitive);

    a = escm_symbol_make(e, str);
    free(str);
    return a;
}

static escm_atom *
symbol_eval(escm *e, escm_tst *sym)
{
    if (!sym->node || !sym->node->atom) {
	escm_error(e, "unknown symbol `~s'.~%", e->curobj);
	escm_abort(e);
    }

    return sym->node->atom;
}

static int
symbol_equal(escm *e, escm_tst *t1, escm_tst *t2, int lvl)
{
    (void) e;

    if (lvl == 3)
	return symbol_eq_colorless(t1->symname, t2->symname);
    return t1 == t2;
}

/* XXX: checks for symbol_type, ...? */
static int
symbol_eq_colorless(char *s1, char *s2)
{
    char *t1, *t2;

    t1 = strrchr(s1, '~');
    if (t1 && *(t1 + 1) != '0')
	t1 = NULL;
    t2 = strrchr(s2, '~');
    if (t2 && *(t2 + 1) != '0')
	t2 = NULL;
    if (!t1 && !t2)
	return !strcmp(s1, s2);
    else
	return !strncmp(s1, s2, t1 - s1);
}

static inline int
issymbol(int c)
{
    return (strchr("!$%&*+-./:<=>?@^_~", c) != NULL || isalnum(c));
}
