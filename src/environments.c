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
#include <stdlib.h>

#include "escheme.h"

struct envlist {
    escm_tst *tree;
    escm_tstnode *node;

    struct envlist *next;
};

static unsigned long envtype;

static void env_free(escm_env *);
static void env_mark(escm *, escm_env *);
static void env_print(escm *, escm_env *, escm_output *, int);

static void enter(escm_atom *, int);

void
escm_environments_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) env_mark;
    t->ffree = (Escm_Fun_Free) env_free;
    t->d.c.fprint = (Escm_Fun_Print) env_print;

    envtype = escm_type_add(e, t);
}

void
escm_env_addprimitives(escm *e)
{
    escm_atom *o;

    assert(e != NULL);

    (void) escm_procedure_new(e, "eval", 1, 2, escm_eval, NULL);

    (void) escm_procedure_new(e, "scheme-report-environment", 0, 1,
			      escm_scheme_report_environment, NULL);
    (void) escm_procedure_new(e, "null-environment", 0, 1,
			      escm_null_environment, NULL);
    (void) escm_procedure_new(e, "interaction-environment", 0, 0,
			      escm_interaction_environment, NULL);
    o = escm_procedure_new(e, "alpha", 0, -1, escm_alpha, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;

    o = escm_procedure_new(e, "with", 2, -1, escm_with, NULL);
    escm_proc_val(o)->d.c.quoted = 0x6;
}

size_t
escm_env_tget(void)
{
    return envtype;
}

escm_atom *
escm_env_new(escm *e, escm_atom *prev)
{
    escm_env *env;

    assert(e != NULL);

    env = xcalloc(1, sizeof *env);
    env->list = NULL;
    env->prev = prev;
    env->tree = (prev && ESCM_ISENV(prev)) ? escm_env_val(prev)->tree : NULL;
    return escm_atom_new(e, envtype, env);
}

void
escm_env_set(escm *e, escm_atom *atomenv, escm_atom *sym, escm_atom *atom)
{
    escm_env *env;
    struct envlist *l;

    assert(atomenv != NULL);
    assert(sym != NULL);

    if (atomenv->ro == 1) {
	escm_error(e, "trying to modify a read-only environment.~%");
	return;
    }

    if (!ESCM_ISENV(atomenv)) {
	escm_error(e, "~s in not an environment.~%", atomenv);
	return;
    }

    env = (escm_env *) atomenv->ptr;

    if (!env->prev) {
	escm_symbol_set(sym, atom);
	return;
    }

    l = xmalloc(sizeof *l);
    l->tree = escm_sym_node(sym);
    l->node = xmalloc(sizeof *l->node);
    l->node->atom = atom;
    l->node->prev = NULL;
    l->next = env->list, env->list = l;

    if (e->env == atomenv) {
	l->node->prev = l->tree->node;
	l->tree->node = l->node;
    }
}

void
escm_env_modify(escm *e, escm_atom *atomenv, escm_atom *sym, escm_atom *atom)
{
    escm_env *env;
    struct envlist *l;

    assert(atomenv != NULL);
    assert(sym != NULL);

    (void) e;

    env = (escm_env *) atomenv->ptr;

    if (!env->prev) {
	escm_symbol_set(sym, atom);
	return;
    }

    for (l = env->list; l; l = l->next) {
	if (l->tree == escm_sym_node(sym)) {
	    l->node->atom = atom;
	    break;
	}
    }
}

escm_atom *
escm_env_enter(escm *e, escm_atom *new)
{
    struct envlist *l;
    escm_atom *a;
    escm_env *env;

    assert(e != NULL);
    assert(new != NULL);

    /* first we mark all the env we want to enter in */
    for (a = new; a; a = escm_env_val(a)->prev)
	a->marked = 1;

    env = new->ptr;

    /* then we leave the non-marked environments */
    for (a = e->env; a && !a->marked; a = escm_env_val(a)->prev) {
	for (l = escm_env_val(a)->list; l; l = l->next)
	    l->tree->node = l->tree->node->prev;
    }
    if (a)
	a->marked = 0;

    /* finally we enter in the new envs */
    enter(new, 0);

    a = e->env;
    e->env = new;
    escm_gc_gard(e, a);
    return a;
}

void
escm_env_leave(escm *e, escm_atom *prevenv)
{
    escm_gc_ungard(e, prevenv);
    escm_gc_ungard(e, escm_env_enter(e, prevenv));
}

/*@-usedef@*/
escm_atom *
escm_eval(escm *e, escm_atom *args)
{
    escm_atom *expr, *env, *prev;

    expr = escm_cons_pop(e, &args);
    env = escm_cons_pop(e, &args);
    if (env) {
	escm_assert(ESCM_ISENV(env), env, e);
	prev = e->env, e->env = env;
    }

    expr = escm_atom_eval(e, expr);
    if (env)
	e->env = prev;

    return expr;
}
/*@=usedef@*/

escm_atom *
escm_alpha(escm *e, escm_atom *args)
{
    escm_atom *env, *prev;

    env = escm_env_new(e, e->env);
    prev = escm_env_enter(e, env);

    (void) escm_begin(e, args);
    if (e->err == 1) {
	escm_env_leave(e, prev);
	return NULL;
    }

    escm_env_leave(e, prev);
    return env;
}

escm_atom *
escm_with(escm *e, escm_atom *args)
{
    escm_atom *env, *prev, *ret;

    env = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISENV(env), env, e);

    escm_env_val(env)->prev = e->env;
    prev = escm_env_enter(e, env);

    ret = escm_begin(e, args);

    escm_env_leave(e, prev);
    return ret;
}

/* XXX: Write this function correctly */
escm_atom *
escm_scheme_report_environment(escm *e, escm_atom *args)
{
    escm_atom *n;
    escm_atom *env;

    n = escm_cons_pop(e, &args);
    if (n) {
	escm_assert(ESCM_ISINT(n), n, e);

	if (escm_number_ival(n) != 5) {
	    escm_error(e, "~s expect 5 as argument.~%", escm_fun(e));
	    escm_abort(e);
	}
    }

    env = e->env;
    while (((escm_env *) env->ptr)->prev)
	env = ((escm_env *) env->ptr)->prev;

    return env; /* return toplevel */
}

escm_atom *
escm_null_environment(escm *e, escm_atom *args)
{
    escm_atom *o;
    escm_atom *env, *prev;

    o = escm_cons_pop(e, &args);
    if (o) {
	escm_assert(ESCM_ISINT(o), o, e);

	if (escm_number_ival(o) != 5) {
	    escm_error(e, "~s expect 5 as argument.~%", escm_fun(e));
	    escm_abort(e);
	}
    }

    env = escm_env_new(e, NULL);
    prev = e->env, e->env = env;
    o = escm_procedure_new(e, "quote", 1, 1, escm_quote, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "quasiquote", 1, 1, escm_quasiquote, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "lambda", 2, -1, escm_lambda, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    e->env = prev;

    return env;
}

escm_atom *
escm_interaction_environment(escm *e, escm_atom *args)
{
    (void) args;

    return e->env;
}

static void
env_mark(escm *e, escm_env *env)
{
    if (!env)
	return;

    if (!env->prev)
	escm_tst_foreach(env->tree, escm_atom_mark, e);
    else {
	struct envlist *l;

	for (l = env->list; l; l = l->next) {
	    if (l->node)
		escm_atom_mark(e, l->node->atom);
	}
    }

    escm_atom_mark(e, env->prev);
}

static void
env_print(escm *e, escm_env *env, escm_output *stream, int lvl)
{
    struct envlist *list;

    (void) e;
    (void) lvl;

    escm_printf(stream, "#<Alpha {");
    for (list = env->list; list; list = list->next) {
	escm_printf(stream, "\"%s\": ", list->tree->symname);
	escm_atom_print3(e, list->node->atom, stream);
	if (list->next)
	    escm_printf(stream, ", ");
    }
    escm_printf(stream, "}>");
}

static void
env_free(escm_env *env)
{
    struct envlist *list, *next;

    assert(env != NULL);

    for (list = env->list; list; list = next) {
	next = list->next;
	free(list->node);
	free(list);
    }

    if (!env->prev) /* only the toplevel can free the tree */
	escm_tst_free(env->tree);

    free(env);
}

static void
enter(escm_atom *atom, int onlyclean)
{
    struct envlist *l;
    escm_env *env;

    if (!atom)
	return;
    if (!atom->marked)
	onlyclean = 1;

    atom->marked = 0;

    env = atom->ptr;

    enter(env->prev, onlyclean);

    if (!onlyclean) {
	for (l = env->list; l; l = l->next) {
	    l->node->prev = l->tree->node;
	    l->tree->node = l->node;
	}
    }
}

