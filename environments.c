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

#include "escheme.h"

struct escm_node {
    char *name;
    escm_atom *atom;

    struct escm_node *next;
};

static size_t envtype;

static void env_free(escm_env *);
static void env_mark(escm *, escm_env *);
static void env_print(escm *, escm_env *, FILE *);

void
escm_environments_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) env_mark;
    t->ffree = (Escm_Fun_Free) env_free;
    t->fprint = (Escm_Fun_Print) env_print;

    envtype = escm_type_add(e, t);
}

void
escm_env_addprimitives(escm *e)
{
    assert(e != NULL);

    (void) escm_procedure_new(e, "eval", 1, 2, escm_eval, NULL);

    (void) escm_procedure_new(e, "scheme-report-environment", 0, 1,
			      escm_scheme_report_environment, NULL);
    (void) escm_procedure_new(e, "null-environment", 0, 1,
			      escm_null_environment, NULL);
    (void) escm_procedure_new(e, "interaction-environment", 0, 0,
			      escm_interaction_environment, NULL);
}

escm_atom *
escm_env_new(escm *e, escm_atom *prev)
{
    escm_env *env;

    assert(e != NULL);

    env = xcalloc(1, sizeof *env);
    if (!prev)
	env->d.toplvl = escm_hash_new(1UL << 10);
    env->prev = prev;

    return escm_atom_new(e, envtype, env);
}

escm_atom *
escm_env_get(escm_atom *atom, const char *name)
{
    escm_env *env;

    assert(atom != NULL);
    assert(name != NULL);

    env = (escm_env *) atom->ptr;

    if (!env->prev)
	return escm_hash_get(env->d.toplvl, name);
    else {
	struct escm_node *n;

	for (n = env->d.lst.first; n; n = n->next) {
	    if (0 == strcmp(n->name, name))
		return n->atom;
	}

	return escm_env_get(env->prev, name);
    }
}

escm_atom *
escm_env_getlocal(escm_atom *atom, const char *name)
{
    escm_env *env;

    assert(atom != NULL);
    assert(name != NULL);

    env = (escm_env *) atom->ptr;

    if (!env->prev)
	return NULL;
    else {
	struct escm_node *n;

	for (n = env->d.lst.first; n; n = n->next) {
	    if (0 == strcmp(n->name, name))
		return n->atom;
	}

	return escm_env_get(env->prev, name);
    }
}

void
escm_env_set(escm_atom *atomenv, const char *name, escm_atom *atom)
{
    escm_env *env;

    assert(atomenv != NULL);
    assert(name != NULL);

    env = (escm_env *) atomenv->ptr;

    if (!env->prev)
	escm_hash_set(env->d.toplvl, name, atom);
    else {
	struct escm_node *n;

	for (n = env->d.lst.first; n; n = n->next) {
	    if (0 == strcmp(n->name, name)) {
		n->atom = atom;
		return;
	    }
	}

	n = xmalloc(sizeof *n);
	n->name = xstrdup(name);
	n->atom = atom;
	n->next = NULL;
	if (env->d.lst.last)
	    env->d.lst.last->next = n, env->d.lst.last = n;
	else
	    env->d.lst.first = n, env->d.lst.last = n;
    }
}

escm_atom *
escm_env_enter(escm *e, escm_atom *new)
{
    escm_atom *ret;

    assert(e != NULL);
    assert(new != NULL);

    ret = e->env, e->env = new;

    escm_gc_gard(e, ret);
    return ret;
}

void
escm_env_leave(escm *e, escm_atom *prevenv)
{
    assert(e != NULL);
    assert(prevenv != NULL);

    escm_gc_ungard(e, prevenv);
    e->env = prevenv;
}

/*@-usedef@*/
escm_atom *
escm_eval(escm *e, escm_atom *args)
{
    escm_atom *expr, *env, *prev;

    expr = escm_cons_pop(e, &args);
    env = escm_cons_pop(e, &args);
    if (env) {
	escm_assert(env->type == envtype, env, e);
	prev = e->env, e->env = env;
    }

    expr = escm_atom_eval(e, expr);
    if (env)
	e->env = prev;

    return expr;
}
/*@=usedef@*/

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
	    fprintf(stderr,"scheme-report-environment expect 5 as argument.\n");
	    e->err = -1;
	    return NULL;
	}
    }

    env = e->env;
    while (((escm_env *) env->ptr)->prev)
	env = ((escm_env *) env->ptr)->prev;

    return env; /* return toplvl */
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
	    fprintf(stderr, "null-environment expect 5 as argument.\n");
	    e->err = -1;
	    return NULL;
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
	escm_hash_foreach(env->d.toplvl, (Escm_Fun_Foreach) escm_atom_mark, e);
    else {
	struct escm_node *n;

	for (n = env->d.lst.first; n; n = n->next)
	    escm_atom_mark(e, n->atom);
    }

    escm_atom_mark(e, env->prev);
}

static void
env_print(escm *e, escm_env *env, FILE *stream)
{
    (void) e;
    (void) env;

    fprintf(stream, "#<Environment>");
}

static void
env_free(escm_env *env)
{
    assert(env != NULL);

    if (!env->prev)
	escm_hash_free(env->d.toplvl);
    else {
	struct escm_node *n, *next;

	for (n = env->d.lst.first; n; n = next) {
	    next = n->next;
	    free(n->name);
	    free(n);
	}
    }

    free(env);
}
