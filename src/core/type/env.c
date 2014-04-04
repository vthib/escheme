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

#include "env.h"
#include "utils.h"
#include "escm.h"
#include "atom.h"
#include "symbols.h"
#include "output.h"

struct escm_envlist {
    escm_tst *tree;
    escm_tstnode *node;

    struct escm_envlist *next;
};

static size_t envtype;

static void env_free(escm_env *);
static void env_mark(escm *, escm_env *);
static void env_print(escm *, escm_env *, escm_output *, int);

static escm_atom *env_enter(escm *, escm_atom *);
static void enterrec(escm_atom *, int);

void
escm_env_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) env_mark;
    t->ffree = (Escm_Fun_Free) env_free;
    t->print.fprint = (Escm_Fun_Print) env_print;

    envtype = escm_type_add(e, t);
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
    return escm_atom_new(e, envtype, env);
}

void
escm_env_set(escm *e, escm_atom *atomenv, escm_atom *sym, escm_atom *atom)
{
    escm_env *env;
    struct escm_envlist *l;

    assert(atomenv != NULL);
    assert(sym != NULL);

    if (atomenv->ro == 1) {
        escm_error(e, _(T("trying to modify a read-only environment.~%")));
        return;
    }

    if (!ESCM_ISENV(atomenv)) {
        escm_error(e, _(T("~s in not an environment.~%")), atomenv);
        return;
    }

    env = (escm_env *) atomenv->ptr;

    if (!env->prev) {
        escm_symbol_set(sym, atom);
        return;
    }

    for (l = env->list; l; l = l->next) {
        if (l->tree == escm_sym_node(sym)) {
            if (!l->node) {
                l->node = xmalloc(sizeof *l->node);
                l->node->prev = NULL;
            }
            l->node->atom = atom;
            return;
        }
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

escm_atom *
escm_env_enter(escm *e, escm_atom *new)
{
    escm_atom *a;

    a = env_enter(e, new);
    escm_gc_gard(e, a);
    return a;
}

void
escm_env_leave(escm *e, escm_atom *prevenv)
{
    escm_gc_ungard(e, prevenv);
    (void) env_enter(e, prevenv);
}

static void
env_mark(escm *e, escm_env *env)
{
    struct escm_envlist *l;

    if (!env)
        return;

    for (l = env->list; l; l = l->next) {
        if (l->node)
            escm_atom_mark(e, l->node->atom);
    }

    escm_atom_mark(e, env->prev);
}

static void
env_print(escm *e, escm_env *env, escm_output *stream, int lvl)
{
    struct escm_envlist *list;

    (void) e;
    (void) lvl;

    if (!env) { /* represent eof_object when characters type is not enabled */
        escm_printf(stream, _(T("#<eof-object>")));
        return;
    }

    escm_printf(stream, _(T("#<Alpha {")));
    for (list = env->list; list; list = list->next) {
        escm_printf(stream, T("\"%") TFMT T("s\": "), list->tree->symname);
        escm_atom_print3(e, list->node->atom, stream);
        if (list->next)
            escm_printf(stream, T(", "));
    }
    escm_printf(stream, T("}>"));
}

static void
env_free(escm_env *env)
{
    struct escm_envlist *list, *next;

    assert(env != NULL);

    for (list = env->list; list; list = next) {
        next = list->next;
        free(list->node);
        free(list);
    }

    free(env);
}

static escm_atom *
env_enter(escm *e, escm_atom *new)
{
    struct escm_envlist *l;
    escm_atom *a;

    assert(e != NULL);
    assert(new != NULL);

    /* first we mark all the env we want to enter in */
    for (a = new; a; a = escm_env_val(a)->prev)
        a->marked = 1;

    /* then we leave the non-marked environments */
    for (a = e->env; a && !a->marked; a = escm_env_val(a)->prev) {
        for (l = escm_env_val(a)->list; l; l = l->next)
            l->tree->node = l->tree->node->prev;
    }
    if (a)
        a->marked = 0;

    /* finally we enter in the new envs */
    enterrec(new, 0);

    a = e->env;
    e->env = new;
    return a;
}

static void
enterrec(escm_atom *atom, int onlyclean)
{
    struct escm_envlist *l;
    escm_env *env;

    if (!atom)
        return;
    if (!atom->marked)
        onlyclean = 1;

    atom->marked = 0;

    env = atom->ptr;

    enterrec(env->prev, onlyclean);

    if (!onlyclean) {
        for (l = env->list; l; l = l->next) {
            l->node->prev = l->tree->node;
            l->tree->node = l->node;
        }
    }
}
