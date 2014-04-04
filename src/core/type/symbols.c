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

#include "symbols.h"
#include "utils.h"
#include "escm.h"
#include "atom.h"
#include "output.h"
#include "input.h"

static size_t symboltype;

static void symbol_print(escm *, escm_tst *, escm_output *, int);
static int symbol_parsetest(escm *, escm_input *, tint);
static escm_atom *symbol_parse(escm *, escm_input *);
static escm_atom *symbol_eval(escm *, escm_tst *);
static void symbol_mark(escm *, escm_tst *);
static int symbol_equal(escm *, escm_tst *, escm_tst *, int);

static inline int issymbol(tint);

void
escm_symbols_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->print.fprint = (Escm_Fun_Print) symbol_print;
    t->parsetest.fparsetest = symbol_parsetest;
    t->parse.fparse = symbol_parse;
    t->eval.feval = (Escm_Fun_Eval) symbol_eval;
    t->equal.fequal = (Escm_Fun_Equal) symbol_equal;
    t->fmark = (Escm_Fun_Mark) symbol_mark;

    symboltype = escm_type_add(e, t);
}

size_t
escm_symbol_tget(void)
{
	return symboltype;
}

escm_atom *
escm_symbol_make(escm *e, const tchar *str)
{
    return escm_atom_new(e, symboltype, escm_tst_gettree(&e->tree, str));
}

escm_atom *
escm_symbol_make2(escm *e, escm_tst *tst)
{
    return escm_atom_new(e, symboltype, tst);
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

static void
symbol_print(escm *e, escm_tst *symbol, escm_output *stream, int lvl)
{
    (void) e;

    if (lvl == 0) {
        escm_print_slashify(stream, symbol->symname);
        return;
    }

    escm_printf(stream, T("%") TFMT T("s"), symbol->symname);
}

static int
symbol_parsetest(escm *e, escm_input *stream, tint c)
{
    (void) e;

    if (c == T('+') || c == T('-')) {
        c = escm_input_getc(stream);
        if (c == T('.')) {
            int ret;

            ret = !isdigit(escm_input_peek(stream));
            escm_input_ungetc(stream, c);
            return ret;
        } else if (c == TEOF)
            return 1;

        escm_input_ungetc(stream, c);
        return !(isdigit(c) || c == T('i'));
    } else if (c == T('.'))
        return !isdigit(escm_input_peek(stream));
    else if (isdigit(c))
        return 0;
    else
        return issymbol(c);
}

static escm_atom *
symbol_parse(escm *e, escm_input *stream)
{
    tchar *str;
    escm_atom *a;

    str = escm_input_getstr_fun(stream, issymbol, e->casesensitive);

    a = escm_symbol_make(e, str);
    free(str);
    return a;
}

static escm_atom *
symbol_eval(escm *e, escm_tst *sym)
{
    if (!sym->node || !sym->node->atom) {
        escm_error(e, _(T("unknown symbol `~s'.~%")), e->curobj);
        escm_abort(e);
    }

    return sym->node->atom;
}

static int
symbol_equal(escm *e, escm_tst *t1, escm_tst *t2, int lvl)
{
    (void) e;
    (void) lvl;

    return t1 == t2;
}

static void
symbol_mark(escm *e, escm_tst *sym)
{
    if (sym->node && sym->node->atom)
        escm_atom_mark(e, sym->node->atom);
}

static inline int
issymbol(tint c)
{
    return (tcschr(T("!$%&*+-./:<=>?@^_~"), c) != NULL || istalnum(c));
}
