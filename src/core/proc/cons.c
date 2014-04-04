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
#include "cons.h"
#include "escm.h"
#include "atom.h"
#include "utils.h"

#include "type/cons.h"
#include "type/procedures.h"

static escm_atom *member(escm *, escm_atom *, int);
static escm_atom *assoc(escm *, escm_atom *, int);

void
escm_addprims_cons(escm *e)
{
    (void) escm_procedure_new(e, T("cons"), 2, 2, escm_prim_cons, NULL);
    (void) escm_procedure_new(e, T("list"), 0, -1, escm_list, NULL);

    (void) escm_procedure_new(e, T("car"), 1, 1, escm_car, NULL);
    (void) escm_procedure_new(e, T("set-car!"), 2, 2, escm_set_car_x, NULL);
    (void) escm_procedure_new(e, T("cdr"), 1, 1, escm_cdr, NULL);
    (void) escm_procedure_new(e, T("set-cdr!"), 2, 2, escm_set_cdr_x, NULL);

    (void) escm_procedure_new(e, T("null?"), 1, 1, escm_null_p, NULL);
    (void) escm_procedure_new(e, T("pair?"), 1, 1, escm_pair_p, NULL);
    (void) escm_procedure_new(e, T("list?"), 1, 1, escm_list_p, NULL);

    (void) escm_procedure_new(e, T("append"), 0, -1, escm_append, NULL);
    (void) escm_procedure_new(e, T("reverse"), 1, 1, escm_reverse, NULL);

    (void) escm_procedure_new(e, T("memq"), 2, 2, escm_memq, NULL);
    (void) escm_procedure_new(e, T("memv"), 2, 2, escm_memv, NULL);
    (void) escm_procedure_new(e, T("member"), 2, 2, escm_member, NULL);

    (void) escm_procedure_new(e, T("assq"), 2, 2, escm_assq, NULL);
    (void) escm_procedure_new(e, T("assv"), 2, 2, escm_assv, NULL);
    (void) escm_procedure_new(e, T("assoc"), 2, 2, escm_assoc, NULL);
}

escm_atom *
escm_prim_cons(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) nil;

    escm_cons_cdr(args) = escm_cons_car(escm_cons_cdr(args));

    return args;
}

escm_atom *
escm_list(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) nil;

    return args;
}

escm_atom *
escm_car(escm *e, escm_atom *args, void *nil)
{
    escm_atom *o;

    (void) nil;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    return escm_cons_car(o);
}

escm_atom *
escm_set_car_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *o;

    (void) nil;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    if (o->ro == 1) {
        escm_error(e, _(T("~s: Can't modify ~s: immutable cons.~%")),
                   escm_fun(e),
                   o);
        escm_abort(e);
    }

    escm_cons_val(o)->car = escm_cons_pop(e, &args);
    return NULL;
}

escm_atom *
escm_cdr(escm *e, escm_atom *args, void *nil)
{
    escm_atom *o;

    (void) nil;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    return escm_cons_cdr(o);
}

escm_atom *
escm_set_cdr_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *o;

    (void) nil;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    if (o->ro == 1) {
        escm_error(e, _(T("~s: Can't modify ~s: immutable cons.~%")),
                        escm_fun(e), o);
        escm_abort(e);
    }

    escm_cons_val(o)->cdr = escm_cons_pop(e, &args);
    return NULL;
}

escm_atom *
escm_null_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;

    return (escm_cons_car(args) == e->NIL) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_pair_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;

    return (ESCM_ISCONS(escm_cons_car(args)) &&
            escm_cons_car(args) != e->NIL) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_list_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *arg;
    escm_cons *c;
#if ESCM_CIRCULAR_LIST >= 1
    escm_cons *end;
#endif
    unsigned int res;

    (void) nil;

    arg = escm_cons_pop(e, &args);

    if (!ESCM_ISCONS(arg))
        res = 0;
    else {
        res = 1;

#if ESCM_CIRCULAR_LIST >= 1
        arg->marked = 1; /* mark all atoms to check circular lists */
        for (c = escm_cons_val(arg), end = c; c; c = escm_cons_next(c),
                 end = c) {
#else
        for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
#endif
            if (!ESCM_ISCONS(c->cdr)
#if ESCM_CIRCULAR_LIST >= 1
                || c->cdr->marked == 1
#endif
                ) {
                res = 0;
                break;
            }
#if ESCM_CIRCULAR_LIST >= 1
            c->cdr->marked = 1;
#endif
        }

#if ESCM_CIRCULAR_LIST >= 1
        arg->marked = 0;
        for (c = escm_cons_val(arg); c != end; c = escm_cons_next(c))
            c->cdr->marked = 0;
#endif
    }

    return (res == 1) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_append(escm *e, escm_atom *args, void *nil)
{
    escm_atom *flist;

    (void) nil;

    escm_ctx_enter(e);
    while (args != e->NIL) {
        flist = escm_cons_pop(e, &args);

        if (args != e->NIL) {
            escm_assert1(ESCM_ISCONS(flist), flist, e, escm_ctx_discard(e));

            escm_ctx_put_splicing(e, flist);
            if (e->err == 1) {
                escm_ctx_discard(e);
                return NULL;
            }
        } else {
            if (!e->ctx->first) {
                escm_ctx_discard(e);
                return flist;
            } else {
                if (!ESCM_ISCONS(e->ctx->last)) {
                    escm_error(e, _(T("~s: could not append to an improper "))
                               _(T("list.~%")), escm_fun(e));
                    escm_ctx_discard(e);
                    escm_abort(e);
                }
                escm_cons_cdr(e->ctx->last) = flist;
            }
        }
    }

    return escm_ctx_leave(e);
}

escm_atom *
escm_reverse(escm *e, escm_atom *args, void *nil)
{
    escm_atom *new, *arg;
    escm_cons *c;

    (void) nil;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    new = e->NIL;
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
        if (!ESCM_ISCONS(c->cdr)) {
            escm_error(e, _(T("~s: ~s: improper list.~%")), escm_fun(e), arg);
            escm_abort(e);
        }
        new = escm_cons_make(e, c->car, new);
    }

    return new;
}

escm_atom *
escm_memq(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return member(e, args, 0);
}

escm_atom *
escm_memv(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return member(e, args, 1);
}

escm_atom *
escm_member(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return member(e, args, 2);
}

escm_atom *
escm_assq(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return assoc(e, args, 0);
}

escm_atom *
escm_assv(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return assoc(e, args, 1);
}

escm_atom *
escm_assoc(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return assoc(e, args, 2);
}

static escm_atom *
member(escm *e, escm_atom *args, int lvl)
{
    escm_atom *elem, *list, *c;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (c = list; c != e->NIL; c = ESCM_ISCONS(escm_cons_cdr(c)) ?
             escm_cons_cdr(c) : e->NIL) {
        if (escm_atom_equal(e, escm_cons_car(c), elem, lvl))
            return c;
    }

    return e->FALSE;
}

static escm_atom *
assoc(escm *e, escm_atom *args, int lvl)
{
    escm_atom *elem, *list, *c;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (c = escm_cons_pop(e, &list); c; c = escm_cons_pop(e, &list)) {
        if (!ESCM_ISCONS(c)) {
            escm_error(e, _(T("~s: ~s is not a c.~%")), escm_fun(e), c);
            escm_abort(e);
        }
        if (escm_atom_equal(e, escm_cons_car(c), elem, lvl))
            return c;
    }

    return e->FALSE;
}
