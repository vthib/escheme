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

#include "procedure.h"
#include "escheme.h"

static void procedure_free(escm_procedure *);
static void procedure_mark(escm *, escm_procedure *);
static void procedure_print(escm *, escm_procedure *, FILE *);
static int procedure_equal(escm *, escm_procedure *, escm_procedure *,
			   unsigned int);

static escm_atom *runprimitive(escm *, escm_atom *, escm_atom *, int);
static escm_atom *runlambda(escm *, escm_atom *, escm_atom *, int);

void
escm_procedure_init(escm *e)
{
    escm_type *t;

    t = xmalloc(sizeof *t);
    t->fmark = (Escm_Fun_Mark) procedure_mark;
    t->ffree = (Escm_Fun_Free) procedure_free;
    t->fprint = (Escm_Fun_Print) procedure_print;
    t->fequal = (Escm_Fun_Equal) procedure_equal;
    t->fparsetest = NULL;
    t->fparse = NULL;
    t->feval = NULL;

    (void) escm_type_add(e, t);

    (void) escm_procedure_new(e, "apply", 2, -1, escm_apply);
}

escm_atom *
escm_procedure_new(escm *e, const char *name, unsigned int min, int max,
		   Escm_Fun_Prim fun)
{
    escm_atom *atom;
    escm_procedure *procedure;

    assert(e != NULL);
    assert(name != NULL);

    procedure = xcalloc(1, sizeof *procedure);
    procedure->type = ESCM_PRIMITIVE;
    procedure->d.c.min = min, procedure->d.c.max = max;
    procedure->d.c.fun = fun;
    procedure->name = xstrdup(name);

    atom = escm_atom_new(e, ESCM_TYPE_PROC, procedure);

    escm_env_set(e->env, name, atom);

    return atom;
}

escm_atom *
escm_procedure_exec(escm *e, escm_atom *atomfun, escm_atom *args, int eval)
{
    if (ESCM_PROC_VAL(atomfun)->type == ESCM_PRIMITIVE)
	return runprimitive(e, atomfun, args, eval);
    else
	return runlambda(e, atomfun, args, eval);
}

escm_atom *
escm_apply(escm *e, escm_atom *args)
{
    escm_atom *fun;
    escm_cons *c, *tail;

    fun = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(fun), fun, e);

    tail = NULL;
    for (c = ESCM_CONS_VAL(args); ESCM_ISCONS(c->cdr) && c->cdr != e->NIL;
	 c = ESCM_CONS_NEXT(c))
	tail = c;
    escm_assert(ESCM_ISCONS(c->car), c->car, e);

    if (tail)
	tail->cdr = c->car;
    else
	args = c->car;

    return escm_procedure_exec(e, fun, args, 0);
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
procedure_print(escm *e, escm_procedure *procedure, FILE *stream)
{
    (void) e;

    assert(procedure != NULL);

    if (procedure->type == ESCM_CLOSURE)
	fprintf(stream, "#<closure>");
    else
	fprintf(stream, "#<primitive %s >", procedure->name);
}

static int
procedure_equal(escm *e, escm_procedure *c1, escm_procedure *c2,
		unsigned int lvl)
{
    (void) e;
    (void) lvl;

    /* the behavior of eq* or not fully specified on procedure. To save memory
       and time, I decide to always return #f if the two procedure are not in
       the same location.
       XXX: maybe recursively check the body of the two procedures */
    return c1 == c2;
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
    args = ESCM_CONS_VAL(atomargs);

    fun = ESCM_PROC_VAL(atomfun);

    escm_ctx_enter(e);

    for (param = 0; args; args = ESCM_CONS_NEXT(args), param++) {
	/* check parameter's number */
	if (fun->d.c.max != -1 && param >= (unsigned int) fun->d.c.max) {
	    escm_atom_display(e, atomfun, stderr);
	    fprintf(stderr, ": too much arguments.\n");
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
		if (!atom || e->err == -1)
		    goto err;
	    }
	}

	escm_ctx_put(e, atom);
    }

    if (param < fun->d.c.min) {
	escm_atom_display(e, atomfun, stderr);
	fprintf(stderr, ": too few arguments.\n");
	goto err;
    }

    atom = fun->d.c.fun(e, escm_ctx_first(e));
    escm_ctx_discard(e);

    return atom;

err:
    escm_ctx_discard(e);
    e->err = -1;
    return NULL;
}

static escm_atom *
runlambda(escm *e, escm_atom *atomfun, escm_atom *atomcons, int eval)
{
    escm_procedure *fun;
    escm_atom *ret;
    escm_atom *prevenv;
    escm_atom *env;
    escm_cons *cons;

    if (!ESCM_ISCONS(atomcons))
	return NULL;
    cons = ESCM_CONS_VAL(atomcons);

    fun = ESCM_PROC_VAL(atomfun);

    if (fun->d.closure.args == e->NIL) /* no arguments, no need to create a
					  new environment */
	prevenv = escm_env_enter(e, fun->d.closure.env);
    else {
	env = escm_env_new(e, fun->d.closure.env);
	escm_gc_gard(e, env);

	if (!ESCM_ISCONS(fun->d.closure.args)) { /* there is one identifier
						    bound on all the args */
	    escm_ctx_enter(e);

	    for (; cons; cons = ESCM_CONS_NEXT(cons)) {
		if (eval) {
		    ret = escm_atom_eval(e, cons->car);
		    if (!ret || e->err == -1) {
			escm_ctx_discard(e);
			goto err;
		    }
		} else
		    ret = cons->car;

		escm_ctx_put(e, ret);
	    }

	    escm_env_set(env, ESCM_SYM_VAL(fun->d.closure.args),
			 escm_ctx_leave(e));
	} else {
	    escm_cons *args;

	    for (args = ESCM_CONS_VAL(fun->d.closure.args); cons;
		 cons = ESCM_CONS_NEXT(cons), args = ESCM_CONS_NEXT(args)) {
		if (!args) {
		    escm_atom_display(e, atomfun, stderr);
		    fprintf(stderr, ": too much arguments.\n");
		    escm_gc_ungard(e, env);
		    goto err;
		}

		if (eval) {
		    ret = escm_atom_eval(e, cons->car);
		    if (!ret || e->err == -1) {
			escm_ctx_discard(e);
			goto err;
		    }
		} else
		    ret = cons->car;

		escm_env_set(env, ESCM_SYM_VAL(args->car), ret);

		if (ESCM_ISSYM(args->cdr)) { /* rest arguments */
		    escm_atom *val;

		    escm_ctx_enter(e);

		    for (cons = ESCM_CONS_NEXT(cons); cons;
			 cons = ESCM_CONS_NEXT(cons)) {
			if (!eval)
			    val = cons->car;
			else
			    val = escm_atom_eval(e, cons->car);
			if (!val || e->err == -1) {
			    escm_ctx_discard(e);
			    escm_gc_ungard(e, env);
			    goto err;
			}
			escm_ctx_put(e, val);
		    }

		    escm_env_set(env, ESCM_SYM_VAL(args->cdr),
				 escm_ctx_leave(e));
		    args = NULL;
		    break;
		}
	    }

	    if (args) {
		escm_atom_display(e, atomfun, stderr);
		fprintf(stderr, ": too few arguments.\n");
		goto err;
	    }
	}

	escm_gc_ungard(e, env);
	prevenv = escm_env_enter(e, env);
    }

    /* now execute */
    ret = NULL;
    for (cons = ESCM_CONS_VAL(fun->d.closure.code); cons;
	 cons = ESCM_CONS_NEXT(cons)) {
	ret = escm_atom_eval(e, cons->car);
	if (e->err == -1) {
	    escm_env_leave(e, prevenv);
	    goto err;
	}
    }

    escm_env_leave(e, prevenv);

    return ret;

err:
    e->err = -1;
    return NULL;
}
