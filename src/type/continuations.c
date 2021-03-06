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
#include <stdio.h>
#include <stdlib.h>

#include "continuations.h"
#include "base.h"

static size_t continuationtype = 0;
static escm_continuation *curcont = NULL;

static void continuation_free(escm_continuation *);
static void continuation_print(escm *, escm_continuation *, escm_output *, int);
static escm_context *context_copy(escm_context *);
static escm_atom *continuation_exec(escm *, escm_continuation *, escm_atom *);

void
escm_continuations_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) continuation_free;
    t->print.fprint = (Escm_Fun_Print) continuation_print;
    t->exec.fexec = (Escm_Fun_Exec) continuation_exec;

    continuationtype = escm_type_add(e, t);

    (void) escm_procedure_new(e, T("continuation?"), 1, 1, escm_continuation_p,
                              NULL);
    (void) escm_procedure_new(e, T("call-with-current-continuation"), 1, 1,
                              escm_call_with_cc, NULL);
    (void) escm_procedure_new(e, T("call/cc"), 1, 1, escm_call_with_cc, NULL);
}

size_t
escm_continuation_tget(void)
{
    return continuationtype;
}

escm_atom *
escm_continuation_p(escm *e, escm_atom *args, void *nil)
{
	(void) nil;

    return (ESCM_ISCONTINUATION(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_call_with_cc(escm *e, escm_atom *args, void *nil)
{
    escm_atom *proc, *cont, *a;
    escm_continuation *c;
    int i;

    (void) nil;

	proc = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(proc), proc, e);

    c = xmalloc(sizeof *c);
    c->ret = NULL;
    cont = escm_atom_new(e, continuationtype, c);

    i = setjmp(c->buf);
    if (i == 0) {
        curcont = c;
        c->ctx = e->ctx, e->ctx = NULL;
        a = escm_procedure_exec(e, proc, escm_cons_make(e, cont, e->NIL), 0);
        e->ctx = context_copy(c->ctx);
        return a;
    } else {
        /* local variable "c" may not be still valable, that why we use a
           global variable "curcont" */
        if (!curcont)
            return NULL;

        e->ctx = context_copy(curcont->ctx);

        return curcont->ret;
    }
}

static void
continuation_free(escm_continuation *c)
{
    escm_context *ctx, *prev;

    for (ctx = c->ctx; ctx; ctx = prev)
        prev = ctx->prev, free(ctx);

    free(c);
}

static void
continuation_print(escm *e, escm_continuation *cont, escm_output *stream,
                   int lvl)
{
    (void) e;
    (void) lvl;
    (void) cont;

    escm_printf(stream, T("#<continuation>"));
}

static escm_context *
context_copy(escm_context *ctx)
{
    escm_context *new, *prev, *ret;

    ret = NULL, prev = NULL;
    for (; ctx; ctx = ctx->prev) {
        new = xcalloc(1, sizeof *new);
        new->first = ctx->first, new->last = ctx->last;
        if (prev)
            prev->prev = new;
        if (!ret)
            ret = new;
        prev = new;
    }

    return ret;
}

static escm_atom *
continuation_exec(escm *e, escm_continuation *cont, escm_atom *args)
{
    cont->ret = escm_atom_eval(e, escm_cons_car(args));
    if (e->err == 1)
        return NULL;

    while (e->ctx)
        escm_ctx_discard(e);

    curcont = cont;
    longjmp(cont->buf, 1);

    return NULL;
}
