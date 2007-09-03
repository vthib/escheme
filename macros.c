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
#include "escheme.h"

static size_t macrotype = 0;

static void macro_mark(escm *, escm_macro *);
static void macro_print(escm *, escm_macro *, FILE *);

void
escm_macros_init(escm *e)
{
    escm_type *t;
    escm_atom *o;

    t = xmalloc(sizeof *t);
    t->fmark = (Escm_Fun_Mark) macro_mark;
    t->ffree = (Escm_Fun_Free) free;
    t->fprint = (Escm_Fun_Print) macro_print;
    t->fequal = NULL;
    t->fparsetest = NULL;
    t->fparse = NULL;
    t->feval = NULL;

    macrotype = escm_type_add(e, t);

    o = escm_procedure_new(e, "syntax-rules", 1, -1, escm_syntax_rules);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "define-syntax", 2, 2, escm_define_syntax);
    escm_proc_val(o)->d.c.quoted = 0x3;
}

size_t
escm_macro_tget(void)
{
    return macrotype;
}

escm_atom *
escm_define_syntax(escm *e, escm_atom *args)
{
    escm_atom *name, *val;

    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(name), name, e);

    val = escm_atom_eval(e, escm_cons_car(args));
    if (e->err == -1)
	return NULL;
    if (!val) {
	escm_atom_display(e, escm_cons_car(args), stderr);
	fprintf(stderr, ": expression not allowed in a definition.\n");
	e->err = -1;
	return NULL;
    }
    escm_assert(ESCM_ISMACRO(val), val, e);

    escm_env_set(e->env, escm_sym_val(name), val);

    return NULL;
}

escm_atom *
escm_syntax_rules(escm *e, escm_atom *args)
{
    escm_macro *m;

    m = xmalloc(sizeof *m);
    m->literals = escm_cons_pop(e, &args);
    escm_assert1(ESCM_ISCONS(m->literals), m->literals, e, free(m));

    m->rules = args;
    m->env = e->env;

    return escm_atom_new(e, macrotype, m);
}

static void
macro_mark(escm *e, escm_macro *m)
{
    escm_atom_mark(e, m->literals);
    escm_atom_mark(e, m->rules);
    escm_atom_mark(e, m->env);
}

static void
macro_print(escm *e, escm_macro *m, FILE *stream)
{
    (void) e;
    (void) m;

    fprintf(stream, "#<macro>");
}
