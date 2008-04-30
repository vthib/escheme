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
#include <assert.h>
#include <string.h>

#include "escheme.h"

static unsigned long proctype = 1; /* uh, small hack */

static void procedure_free(escm_procedure *);
static void procedure_mark(escm *, escm_procedure *);
static void procedure_print(escm *, escm_procedure *, escm_output *, int);
static escm_atom *procedure_exec(escm *, escm_procedure *, escm_atom *);
 
static escm_atom *runprimitive(escm *, escm_atom *, escm_atom *, int);
static escm_atom *runlambda(escm *, escm_atom *, escm_atom *, int);

static escm_atom *foreach(escm *, escm_atom *, int);

void
escm_procedures_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) procedure_mark;
    t->ffree = (Escm_Fun_Free) procedure_free;
    t->d.c.fprint = (Escm_Fun_Print) procedure_print;
    t->d.c.fexec = (Escm_Fun_Exec) procedure_exec;

    proctype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "procedure?", 1, 1, escm_procedure_p, NULL);

    (void) escm_procedure_new(e, "apply", 2, -1, escm_apply, NULL);
    (void) escm_procedure_new(e, "map", 2, -1, escm_map, NULL);
    (void) escm_procedure_new(e, "for-each", 2, -1, escm_for_each, NULL);
}

unsigned long
escm_proc_tget(void)
{
    return proctype;
}

escm_atom *
escm_procedure_new(escm *e, const char *name, unsigned int min, int max,
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
    procedure->name = xstrdup(name);

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

escm_atom *
escm_procedure_p(escm *e, escm_atom *args)
{
    return ESCM_ISPROC(escm_cons_pop(e, &args)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_apply(escm *e, escm_atom *args)
{
    escm_atom *fun;
    escm_cons *c, *tail;

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

    if (!escm_tailrec3(e, fun, args, 0))
	return NULL;
    return escm_procedure_exec(e, fun, args, 0);
}

escm_atom *
escm_map(escm *e, escm_atom *args)
{
    return foreach(e, args, 1);
}

escm_atom *
escm_for_each(escm *e, escm_atom *args)
{
    return foreach(e, args, 0);
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
	    escm_printf(stream, "#<closure %s>", procedure->name);
	else
	    escm_printf(stream, "#<closure>", procedure->name);
    } else
	escm_printf(stream, "#<primitive %s>", procedure->name);
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

    for (param = 0; args; args = escm_cons_next(args), param++) {
	/* check parameter's number */
	if (fun->d.c.max != -1 && param >= (unsigned int) fun->d.c.max) {
	    escm_error(e, "~s: too much arguments.~%", atomfun);
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
		    escm_error(e, "~s: ~s must return a real value.~%", atomfun,
			       args->car);
		    goto err;
		}
	    }
	}

	escm_ctx_put(e, atom);
    }

    if (param < fun->d.c.min) {
	escm_error(e, "~s: too few arguments.~%", atomfun);
	goto err;
    }

    e->ctx->last = e->env; /* just a hack to indicate to the backtrace printer
			      that the primitive is running */

    if (fun->d.c.data) {
	escm_atom *(*f)(escm *, escm_atom *, void *);

	f = (escm_atom *(*)(escm *, escm_atom *, void *)) fun->d.c.fun;
	atom = f(e, escm_ctx_first(e), fun->d.c.data);
    } else
	atom = fun->d.c.fun(e, escm_ctx_first(e));

    escm_ctx_discard(e);

    return atom;

err:
    escm_ctx_discard(e);
    escm_abort(e);
}

static escm_atom *
runlambda(escm *e, escm_atom *volatile atomfun, escm_atom *atomargs, int eval)
{
    escm_procedure *volatile fun;
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
    if (setjmp(e->ctx->jbuf) != 0) {
	struct escm_slist *li, *lprev;

	/* atomfun = e->ctx->fun; */
	escm_env_leave(e, prevenv);

	/* clean the gc gards */
	for (li = e->gard; li && li->atom != lastgarded; li = lprev)
	    lprev = li->prev, free(li);
	e->gard = li;

	/* escm_notice(e, "~s receive local jump with args ~s.~%", atomfun,
	   e->ctx->first);*/
    } else {
	escm_atom *atom;

	if (atomargs == e->NIL)
	    atomargs = NULL;
	while (atomargs) {
	    if (!eval)
		atom = escm_cons_pop(e, &atomargs);
	    else {
		atom = escm_atom_eval(e, escm_cons_pop(e, &atomargs));
		if (!atom || e->err == 1)
		    goto erreval;
	    }

	    escm_ctx_put(e, atom);
	}

	e->ctx->fun = atomfun;
	lastgarded = e->gard->atom;
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
		     args = escm_cons_val(e->ctx->first); args;
		 funargs = escm_cons_next(funargs),
		     args = escm_cons_next(args)) {
		if (!funargs) {
		    escm_error(e, "~s: too much arguments.~%", atomfun);
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
		escm_error(e, "~s: too few arguments.~%", atomfun);
		goto erreval;
	    }
	}

	prevenv = escm_env_enter(e, env);
    }

    /* now execute */
    ret = NULL;
    unsigned int tmp;

    tmp = e->tailrec;
    for (cons = escm_cons_val(fun->d.closure.code); cons;
	 cons = escm_cons_next(cons)) {
	if (cons->cdr == e->NIL) {
	    if (tmp != 2)
		e->tailrec = 1;
	    ret = escm_atom_eval3(e, cons->car, 1);
	} else
	    ret = escm_atom_eval(e, cons->car);

	if (e->err == 1) {
	    escm_env_leave(e, prevenv);
	    goto erreval;
	}
    }
    e->tailrec = tmp;

    escm_ctx_discard(e);
    escm_env_leave(e, prevenv);

    return ret;

erreval:
    escm_ctx_discard(e);
    escm_abort(e);
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
	    if (!c->car || c->car == e->NIL) {
		escm_cons *c2;

		if (c != escm_cons_val(args)) /* not the first list, so they
						 dont have same length */
		    goto err_length;
		for (c2 = escm_cons_next(c); c2; c2 = escm_cons_next(c2)) {
		    if (c2->car != NULL)
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
		escm_error(e, "~s: the procedure must yeild a value.~%",
			   escm_fun(e));
	    }
	} else /* for-each version */
	    (void) escm_procedure_exec(e, proc, escm_ctx_leave(e), 0);
    }

err_length:
    escm_error(e, "~s: all lists must have the same length.~%", escm_fun(e));
    escm_ctx_discard(e), escm_ctx_discard(e);
    escm_abort(e);
}
