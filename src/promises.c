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

#include "escheme.h"

static unsigned long promisetype = 0;

static void promise_mark(escm *, escm_atom *);
static void promise_print(escm *, escm_atom *, escm_output *, int);

void
escm_promises_init(escm *e)
{
    escm_type *t;
    escm_atom *a;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) promise_mark;
    t->d.c.fprint = (Escm_Fun_Print) promise_print;

    promisetype = escm_type_add(e, t);

    a = escm_procedure_new(e, "delay", 1, 1, escm_delay, NULL);
    escm_proc_val(a)->d.c.quoted = 0x1;
    (void) escm_procedure_new(e, "force", 1, 1, escm_force, NULL);
}

size_t
escm_promise_tget(void)
{
    return promisetype;
}

escm_atom *
escm_delay(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_atom_new(e, promisetype, escm_cons_pop(e, &args));
    a->env = e->env;

    return a;
}

escm_atom *
escm_force(escm *e, escm_atom *args)
{
    escm_atom *promise, *penv;
    escm_atom *atom;

    promise = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROMISE(promise), promise, e);

    if (!promise->env) /* value already computed */
	return promise->ptr;
    penv = escm_env_enter(e, promise->env);

    atom = escm_atom_eval(e, promise->ptr);
    escm_env_leave(e, penv);
    if (e->err == 1)
	return NULL;

    promise->ptr = atom;
    promise->env = NULL;
    return atom;
}

static void
promise_mark(escm *e, escm_atom *p)
{
    escm_atom_mark(e, p);
}

static void
promise_print(escm *e, escm_atom *p, escm_output *stream, int lvl)
{
    (void) e;
    (void) p;
    (void) lvl;

    escm_scmpf(e, stream, "#<promise ~s>", p);
}
