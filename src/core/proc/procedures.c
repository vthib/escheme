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
#include "procedures.h"
#include "escm.h"
#include "atom.h"
#include "utils.h"

#include "type/procedures.h"
#include "type/cons.h"

static escm_atom *foreach(escm *e, escm_atom *args, int map);

void
escm_addprims_procedure(escm *e)
{
    (void) escm_procedure_new(e, T("procedure?"), 1, 1, escm_procedure_p, NULL);

    (void) escm_procedure_new(e, T("apply"), 2, -1, escm_apply, NULL);
    (void) escm_procedure_new(e, T("map"), 2, -1, escm_map, NULL);
    (void) escm_procedure_new(e, T("for-each"), 2, -1, escm_for_each, NULL);
}

escm_atom *
escm_procedure_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return ESCM_ISPROC(escm_cons_pop(e, &args)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_apply(escm *e, escm_atom *args, void *nil)
{
    escm_atom *fun;
    escm_cons *c, *tail;

    (void) nil;
    fun = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(fun), fun, e);

    tail = NULL;
    for (c = escm_cons_val(args); c->cdr != e->NIL; c = escm_cons_next(c))
        tail = c;
    escm_assert(ESCM_ISCONS(c->car), c->car, e);

    if (tail)
        tail->cdr = c->car;
    else
        args = c->car;

    if (!escm_tailrec4(e, fun, args, 0))
        return NULL;
    return escm_procedure_exec(e, fun, args, 0);
}

escm_atom *
escm_map(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return foreach(e, args, 1);
}

escm_atom *
escm_for_each(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return foreach(e, args, 0);
}

static escm_atom *
foreach(escm *e, escm_atom *args, int map)
{
    escm_atom *proc, *atom;
    escm_cons *c;

    proc = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(proc), proc, e);

    for (c = escm_cons_val(args); c; c = escm_cons_next(c))
        escm_assert(ESCM_ISCONS(c->car), c->car, e);

    escm_ctx_enter(e);

    for (;;) {
        escm_ctx_enter(e);

        for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
            if (c->car == e->NIL) {
                escm_cons *c2;

                if (c != escm_cons_val(args)) /* not the first list, so they
                                                 dont have same length */
                    goto err_length;
                for (c2 = escm_cons_next(c); c2; c2 = escm_cons_next(c2)) {
                    if (c2->car != e->NIL)
                        goto err_length;
                }
                escm_ctx_discard(e);
                return escm_ctx_leave(e);
            }
            escm_ctx_put(e, escm_cons_pop(e, &c->car));
        }
        if (map) { /* map version: build a list of the results */
            atom = escm_procedure_exec(e, proc, escm_ctx_leave(e), 0);
            if (atom)
                escm_ctx_put(e, atom);
            else {
                if (e->err == 1) {
                    escm_ctx_discard(e);
                    return NULL;
                }
                escm_error(e, _(T("~s: the procedure must yeild a value.~%")),
                           escm_fun(e));
            }
        } else /* for-each version */
            (void) escm_procedure_exec(e, proc, escm_ctx_leave(e), 0);
    }

err_length:
    escm_error(e, _(T("~s: all lists must have the same length.~%")),
               escm_fun(e));
    escm_ctx_discard(e), escm_ctx_discard(e);
    escm_abort(e);
}
