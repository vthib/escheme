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

#include "procedures.h"
#include "utils.h"
#include "escm.h"
#include "atom.h"
#include "env.h"
#include "symbols.h"
#include "output.h"
#include "cons.h"

static size_t proctype;

static void procedure_free(escm_procedure *);
static void procedure_mark(escm *, escm_procedure *);
static void procedure_print(escm *, escm_procedure *, escm_output *, int);
static escm_atom *procedure_exec(escm *, escm_procedure *, escm_atom *);

static escm_atom *runprimitive(escm *, escm_atom *, escm_atom *, int);
static escm_atom *runlambda(escm *, escm_atom *, escm_atom *, int);

void
escm_procedures_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) procedure_mark;
    t->ffree = (Escm_Fun_Free) procedure_free;
    t->print.fprint = (Escm_Fun_Print) procedure_print;
    t->exec.fexec = (Escm_Fun_Exec) procedure_exec;

    proctype = escm_type_add(e, t);
}

size_t
escm_proc_tget(void)
{
	return proctype;
}

escm_atom *
escm_procedure_new(escm *e, const tchar *name, unsigned int min, int max,
                   Escm_Fun_Prim fun, void *data)
{
    escm_atom *atom;
    escm_procedure *procedure;

    assert(e != NULL);
    assert(name != NULL);

    procedure = xcalloc(1, sizeof *procedure);
    procedure->type = ESCM_PRIMITIVE;
    procedure->d.c.min = min, procedure->d.c.max = max;
    procedure->d.c.fun = fun;
    procedure->d.c.data = data;
    procedure->name = tcsdup(name);

    atom = escm_atom_new(e, proctype, procedure);

    escm_env_set(e, e->env, escm_symbol_make(e, name), atom);

    return atom;
}

escm_atom *
escm_procedure_exec(escm *e, escm_atom *atomfun, escm_atom *args, int eval)
{
    if (escm_proc_val(atomfun)->type == ESCM_PRIMITIVE)
        return runprimitive(e, atomfun, args, eval);
    else
        return runlambda(e, atomfun, args, eval);
}

static void
procedure_free(escm_procedure *procedure)
{
    assert(procedure != NULL);

    free(procedure->name);
    free(procedure);
}

static void
procedure_mark(escm *e, escm_procedure *proc)
{
    assert (proc != NULL);

    if (proc->type == ESCM_CLOSURE) {
        escm_atom_mark(e, proc->d.closure.env);
        escm_atom_mark(e, proc->d.closure.code);
        escm_atom_mark(e, proc->d.closure.args);
    }
}

static void
procedure_print(escm *e, escm_procedure *procedure, escm_output *stream,
                int lvl)
{
    (void) e;
    (void) lvl;

    assert(procedure != NULL);

    if (procedure->type == ESCM_CLOSURE) {
        if (procedure->name)
            escm_printf(stream, T("#<closure %") TFMT T("s>"), procedure->name);
        else
            escm_printf(stream, T("#<closure>"), procedure->name);
    } else
        escm_printf(stream, T("#<primitive %") TFMT T("s>"), procedure->name);
}

static escm_atom *
procedure_exec(escm *e, escm_procedure *proc, escm_atom *args)
{
    (void) proc;

    return escm_procedure_exec(e, e->curobj, args, 1);
}

static escm_atom *
runprimitive(escm *e, escm_atom *atomfun, escm_atom *atomargs, int eval)
{
    escm_procedure *fun;
    escm_atom *atom;
    unsigned int param;
    escm_cons *args;

    if (!ESCM_ISCONS(atomargs))
        return NULL;
    args = escm_cons_val(atomargs);

    fun = escm_proc_val(atomfun);

    escm_ctx_enter(e);
    e->ctx->fun = atomfun;
    e->ctx->tailrec = 0;

    for (param = 0; args; args = escm_cons_next(args), param++) {
        /* check parameter's number */
        if (fun->d.c.max != -1 && param >= (unsigned int) fun->d.c.max) {
            escm_error(e, _(T("~s: too much arguments.~%")), atomfun);
            goto err;
        }

        if (!eval)
            atom = args->car;
        else {
            if ((param > fun->d.c.min && fun->d.c.max == -1 &&
                 ((fun->d.c.quoted >> fun->d.c.min) & 0x1)) ||
                ((fun->d.c.quoted >> param) & 0x1))
                atom = args->car;
            else {
                atom = escm_atom_eval(e, args->car);
                if (e->err == 1)
                    goto err;
                if (!atom) {
                    escm_error(e, _(T("~s: ~s must return a real value.~%")),
                               atomfun, args->car);
                    goto err;
                }
            }
        }

        escm_ctx_put(e, atom);
    }

    if (param < fun->d.c.min) {
        escm_error(e, _(T("~s: too few arguments.~%")), atomfun);
        goto err;
    }

    e->ctx->last = e->env; /* just a hack to indicate to the backtrace printer
                              that the primitive is running */
    atom = fun->d.c.fun(e, escm_ctx_first(e), fun->d.c.data);

    escm_ctx_discard(e);

    return atom;

err:
    escm_ctx_discard(e);
    escm_abort(e);
}

static escm_atom *
runlambda(escm *e, escm_atom *atomfun, escm_atom *atomargs, int eval)
{
    escm_procedure *fun;
    escm_atom *ret;
    escm_atom *volatile prevenv;
    escm_atom *volatile lastgarded;
    escm_atom *env;
    escm_cons *cons;

    if (!ESCM_ISCONS(atomargs))
        return NULL;

    env = NULL, prevenv = NULL;
    fun = escm_proc_val(atomfun);

    escm_ctx_enter(e);
    e->ctx->fun = atomfun;
    e->ctx->tailrec = 0;

    if (setjmp(e->ctx->jbuf) != 0) {
        struct escm_slist *li, *lprev;

        /*escm_notice(e, "~s receive local jump with args ~s for ~s.~%",
          atomfun, e->ctx->first, e->ctx->fun);*/

        atomfun = e->ctx->fun;
        fun = escm_proc_val(atomfun);
        escm_env_leave(e, prevenv);

        /* clean the gc gards */
        if (lastgarded != NULL) {
            for (li = e->gard; li && li->atom != lastgarded; li = lprev)
                lprev = li->prev, free(li);
            e->gard = li;
        }
    } else {
        escm_atom *atom;

        while (atomargs != e->NIL) {
            ret = escm_cons_pop(e, &atomargs);
            if (!eval)
                escm_ctx_put(e, ret);
            else {
                atom = escm_atom_eval(e, ret);
                if (!atom) {
                    escm_error(e, _(T("~s: ~s must return a real value.~%")),
                               atomfun, ret);
                    e->err = 1;
                }
                if (e->err == 1)
                    goto erreval;
                escm_ctx_put(e, atom);
            }
        }

        lastgarded = (e->gard) ? e->gard->atom : NULL;
    }

    if (fun->d.closure.args == e->NIL) /* no arguments, no need to create a
                                          new environment */
        prevenv = escm_env_enter(e, fun->d.closure.env);
    else {
        env = escm_env_new(e, fun->d.closure.env);

        if (!ESCM_ISCONS(fun->d.closure.args)) { /* there is one identifier
                                                    bound on all the args */
            escm_env_set(e, env, fun->d.closure.args, escm_ctx_first(e));
        } else {
            escm_cons *funargs, *args;

            for (funargs = escm_cons_val(fun->d.closure.args),
                     args = e->ctx->first ? escm_cons_val(e->ctx->first) :
                     NULL;
                 args; funargs = escm_cons_next(funargs),
                     args = escm_cons_next(args)) {
                if (!funargs) {
                    escm_error(e, _(T("~s: too much arguments.~%")), atomfun);
                    goto erreval;
                }

                escm_env_set(e, env, funargs->car, args->car);

                if (ESCM_ISSYM(funargs->cdr)) { /* rest arguments */
                    escm_env_set(e, env, funargs->cdr, args->cdr);
                    funargs = NULL;
                    break;
                }
            }
            if (funargs) {
                escm_error(e, _(T("~s: too few arguments.~%")), atomfun);
                goto erreval;
            }
        }

        prevenv = escm_env_enter(e, env);
    }

    /* now execute */
    ret = NULL;

    for (cons = escm_cons_val(fun->d.closure.code); cons;
         cons = escm_cons_next(cons)) {
        if (cons->cdr == e->NIL) {
            e->ctx->tailrec = 1;
            ret = escm_atom_eval(e, cons->car);
        } else
            ret = escm_atom_eval(e, cons->car);

        if (e->err == 1) {
            escm_env_leave(e, prevenv);
            goto erreval;
        }
    }

    escm_ctx_discard(e);
    escm_env_leave(e, prevenv);

    return ret;

erreval:
    escm_ctx_discard(e);
    escm_abort(e);
}
