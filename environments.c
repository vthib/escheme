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

typedef struct escm_tst escm_tst;

struct escm_tst {
    escm_tst *lo, *hi, *down;
    char cval;
    escm_atom *atom;
};

static size_t envtype;

static void env_free(escm_env *);
static void env_mark(escm *, escm_env *);
static void env_print(escm *, escm_env *, escm_output *, int);

static void tst_set(escm_tst **, const char *, escm_atom *);
static int tst_edit(escm_tst **, const char *, escm_atom *);
static escm_atom *tst_get(escm_tst *, const char *);
static void tst_foreach(escm_tst *, void (*)(escm *, escm_atom *), escm *);
static void tst_free(escm_tst *);

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

    env = xmalloc(sizeof *env);
    env->tree = NULL;
    env->prev = prev;

    return escm_atom_new(e, envtype, env);
}

escm_atom *
escm_env_get(escm_atom *atom, const char *name)
{
    escm_env *env;
    escm_atom *a;

    assert(name != NULL);

    if (!atom)
	return NULL;

    env = (escm_env *) atom->ptr;

    a = tst_get(env->tree, name);
    if (!a)
	return escm_env_get(env->prev, name);
    else
	return a;
}

escm_atom *
escm_env_getlocal(escm_atom *atom, const char *name)
{
    escm_env *env;
    escm_atom *a;

    assert(name != NULL);

    if (!atom)
	return NULL;

    env = (escm_env *) atom->ptr;

    if (!env->prev)
	return NULL;

    a = tst_get(env->tree, name);
    if (!a)
	return escm_env_get(env->prev, name);
    else
	return a;
}

void
escm_env_set(escm_atom *atomenv, const char *name, escm_atom *atom)
{
    escm_env *env;

    assert(atomenv != NULL);
    assert(name != NULL);

    env = (escm_env *) atomenv->ptr;

    tst_set(&env->tree, name, atom);
}

void
escm_env_edit(escm_atom *atomenv, const char *name, escm_atom *atom)
{
    escm_env *env;

    assert(atomenv != NULL);
    assert(name != NULL);

    env = (escm_env *) atomenv->ptr;

    if (!tst_edit(&env->tree, name, atom))
	escm_env_edit(env->prev, name, atom);
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
	    fprintf(stderr,"scheme-report-environment expect 5 as argument.\n");
	    e->err = 1;
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
	    e->err = 1;
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

    tst_foreach(env->tree, escm_atom_mark, e);
    escm_atom_mark(e, env->prev);
}

static void
env_print(escm *e, escm_env *env, escm_output *stream, int lvl)
{
    (void) e;
    (void) env;
    (void) lvl;

    escm_printf(stream, "#<Alpha>");
}

static void
env_free(escm_env *env)
{
    assert(env != NULL);

    tst_free(env->tree);
    free(env);
}

/* Ternary search tree */

static void
tst_set(escm_tst **t, const char *s, escm_atom *atom)
{
    if (*s == '\0')
        return;

    if (!*t) {
        *t = xcalloc(1, sizeof **t);
        (*t)->cval = *s;
    }

    if (*s < (*t)->cval)
        tst_set(&(*t)->lo, s, atom);
    else if (*s > (*t)->cval)
        tst_set(&(*t)->hi, s, atom);
    else {
        if (*(s + 1) == '\0')
            (*t)->atom = atom;
	else
	    tst_set(&(*t)->down, s + 1, atom);
    }
}

static int
tst_edit(escm_tst **t, const char *s, escm_atom *atom)
{
    if (*s == '\0')
        return 0;

    if (!*t) {
	if (*(s + 1) == '\0')
	    return 0;
        *t = xcalloc(1, sizeof **t);
        (*t)->cval = *s;
    }

    if (*s < (*t)->cval)
        return tst_edit(&(*t)->lo, s, atom);
    else if (*s > (*t)->cval)
        return tst_edit(&(*t)->hi, s, atom);
    else {
        if (*(s + 1) == '\0') {
	    if ((*t)->atom == NULL)
		return 0;
	    (*t)->atom = atom;
	    return 1;
	} else
	    return tst_edit(&(*t)->down, s + 1, atom);
    }
}

static escm_atom *
tst_get(escm_tst *t, const char *s)
{
    if (!t)
        return NULL;

    if (*s < t->cval)
        return tst_get(t->lo, s);
    else if (*s > t->cval)
        return tst_get(t->hi, s);
    else {
        if (*(s + 1) == '\0')
            return t->atom;
        else
	    return tst_get(t->down, s + 1);
    }
}

static void
tst_foreach(escm_tst *t, void (*f)(escm *, escm_atom *), escm *e)
{
    if (!t)
        return;

    (*f)(e, t->atom);

    tst_foreach(t->lo, f, e);
    tst_foreach(t->down, f, e);
    tst_foreach(t->hi, f, e);
}

static void
tst_free(escm_tst *t)
{
    if (!t)
        return;

    tst_free(t->lo);
    tst_free(t->down);
    tst_free(t->hi);

    free(t);
}
