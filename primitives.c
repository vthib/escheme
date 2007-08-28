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

static escm_atom *named_let(escm *e, escm_atom *, escm_atom *);

void
escm_primitives_load(escm *e)
{
    escm_atom *o;

    o = escm_procedure_new(e, "quote", 1, 1, escm_quote);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "quasiquote", 1, 1, escm_quasiquote);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "unquote", 1, 1, escm_unquote);
    escm_proc_val(o)->d.c.quoted = 0x1;

    e->LAMBDA = escm_procedure_new(e, "lambda", 2, -1, escm_lambda);
    escm_proc_val(e->LAMBDA)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, "define", 2, 2, escm_define);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "set!", 2, 2, escm_set_x);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, "let", 2, -1, escm_let);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, "let*", 2, -1, escm_let_star);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, "letrec", 2, -1, escm_letrec);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, "if", 2, 3, escm_if);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, "cond", 1, -1, escm_cond);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "case", 2, -1, escm_case);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, "and", 0, -1, escm_and);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "or", 0, -1, escm_or);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, "begin", 1, -1, escm_begin);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "do", 2, -1, escm_do);
    escm_proc_val(o)->d.c.quoted = 0x7;

    (void) escm_procedure_new(e, "eqv?", 2, 2, escm_eqv_p);
    (void) escm_procedure_new(e, "eq?", 2, 2, escm_eq_p);
    (void) escm_procedure_new(e, "equal?", 2, 2, escm_equal_p);
}

escm_atom *
escm_quote(escm *e, escm_atom *arg)
{
    assert(e != NULL);

    escm_cons_val(arg)->car->ro = 1;

    return escm_cons_val(arg)->car;
}

escm_atom *
escm_quasiquote(escm *e, escm_atom *args)
{
    escm_atom *arg, *pair, *a;
    escm_cons *c;

    arg = escm_cons_pop(e, &args);
    if (!ESCM_ISCONS(arg)) {
	arg->ro = 1;
	return arg;
    }

    escm_ctx_enter(e);
    e->ctx->quasiquote = 1;

    pair = escm_cons_new(e, NULL, e->NIL); /* use for recursion */
    escm_gc_gard(e, pair);

    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
	if (!ESCM_ISCONS(c->cdr)) {
	    e->dotted = 1;
	    escm_ctx_put(e, c->cdr);
	} else if (ESCM_ISSYM(c->car)) {
	    if (0 == strcmp(escm_sym_val(c->car), "unquote")) {
		a = escm_unquote(e, c->cdr);
		if (!a)
		    goto err;
		e->dotted = 1;
		escm_ctx_put(e, a);
	    } else if (0 == strcmp(escm_sym_val(c->car), "unquote-splicing")) {
		fprintf(stderr, "unquote-splicing found after a dotted "
			"notation.\n");
		goto err;
	    } else
		goto add;
	} else if (ESCM_ISCONS(c->car) && c->car != e->NIL) {
	    escm_atom *list;

	    list = c->car;
	    a = escm_cons_pop(e, &list);
	    if (ESCM_ISSYM(a)) {
		if (0 == strcmp(escm_sym_val(a), "unquote")) {
 		    a = escm_unquote(e, list);
		    if (!a)
			goto err;
		    escm_ctx_put(e, a);
		} else if (0 == strcmp(escm_sym_val(a), "unquote-splicing")) {
 		    a = escm_unquote(e, list);
		    if (!a)
			goto err;
		    else if (!ESCM_ISCONS(a)) {
			fprintf(stderr, "unquote-splicing expect a argument of "
				"type <list>.\n");
			goto err;
		    }
		    escm_ctx_put_splicing(e, a);
		} else
		    goto rec;
	    } else {
	    rec:
		escm_cons_val(pair)->car = c->car;
		escm_ctx_put(e, escm_quasiquote(e, pair));
	    }
	} else {
	add:
	    escm_ctx_put(e, c->car);
	}
    }

    escm_gc_ungard(e, pair);
    return escm_ctx_leave(e);

err:
    escm_ctx_discard(e);
    return NULL;
}

escm_atom *
escm_unquote(escm *e, escm_atom *args)
{
    escm_atom *arg, *res;

    if (!e->ctx || e->ctx->quasiquote == 0) {
	fprintf(stderr, "error: unquote not allowed outside a quasiquote.\n");
	e->err = -1;
	return NULL;
    }

    arg = escm_cons_pop(e, &args);
    res = escm_atom_eval(e, arg);
    if (!res) {
	if (e->err != -1) {
	    escm_atom_display(e, arg, stderr);
	    fprintf(stderr, ": expression not allowed in this context.\n");
	    e->err = -1;
	}
	return NULL;
    }

    return res;
}

escm_atom *
escm_lambda(escm *e, escm_atom *args)
{
    escm_atom *params;
    escm_procedure *lambda;

    params = escm_cons_pop(e, &args);

    escm_assert(ESCM_ISCONS(params) || ESCM_ISSYM(params), params, e);
 
    if (ESCM_ISCONS(params)) { /* verify that all identifiers are symbols */
	escm_cons *c;

	for (c = escm_cons_val(params); c; c = escm_cons_next(c)) {
	    escm_assert(ESCM_ISSYM(c->car), c->car, e);
	    if (!ESCM_ISCONS(c->cdr))
		escm_assert(ESCM_ISSYM(c->cdr), c->cdr, e);
	}
    }

    lambda = xcalloc(1, sizeof *lambda);
    lambda->type = ESCM_CLOSURE;
    lambda->d.closure.args = params;
    lambda->d.closure.env = e->env;
    lambda->d.closure.code = args;

    return escm_atom_new(e, ESCM_TYPE_PROC, lambda);
}

escm_atom *
escm_define(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);

    escm_assert(ESCM_ISSYM(c) || ESCM_ISCONS(c), c, e);

    if (ESCM_ISCONS(c)) {
	escm_cons *a;

	a = escm_cons_val(c);
	escm_assert(a != NULL && ESCM_ISSYM(a->car), c, e);

	escm_ctx_enter(e);
	escm_ctx_put(e, a->cdr); /* formals */
	escm_ctx_put(e, escm_cons_pop(e, &args)); /* body */

	escm_env_set(e->env, escm_sym_val(a->car),
		     escm_procedure_exec(e, e->LAMBDA, escm_ctx_first(e), 0));
	escm_ctx_discard(e);
    } else {
	escm_atom *val;

	escm_gc_gard(e, c);
	val = escm_atom_eval(e, escm_cons_pop(e, &args));
	escm_gc_ungard(e, c);
	if (!val)
	    return NULL;

	escm_env_set(e->env, escm_sym_val(c), val);
    }

    return NULL;
}

escm_atom *
escm_set_x(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);

    escm_assert(ESCM_ISSYM(c), c, e);

    if (!escm_env_get(e->env, escm_sym_val(c))) {
	escm_atom_display(e, c, stderr);
	fprintf(stderr, ": unknown identifier.\n");
	e->err = -1;
	return NULL;
    }

    escm_env_set(e->env, escm_sym_val(c), escm_cons_pop(e, &args));

    return NULL;
}

escm_atom *
escm_let(escm *e, escm_atom *args)
{
    escm_atom *env, *prevenv, *arg, *varname, *varval, *ret;
    escm_cons *c, *varcons;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg) || ESCM_ISSYM(arg), arg, e);

    if (ESCM_ISSYM(arg))
	return named_let(e, arg, args);

    /* we first create a new empty environment */
    env = escm_env_new(e, e->env);
    escm_gc_gard(e, env);

    /* then we bind each variable in this new environment */
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
	escm_assert1(ESCM_ISCONS(c->car), c->car, e, escm_gc_ungard(e, env));

	varcons = escm_cons_val(c->car);
	escm_assert1(varcons, c->car, e, escm_gc_ungard(e, env));

	varname = varcons->car, varcons = escm_cons_next(varcons);
	escm_assert1(ESCM_ISSYM(varname), c->car, e, escm_gc_ungard(e, env));
	escm_assert1(varcons != NULL && varcons->cdr == e->NIL, c->car, e,
		     escm_gc_ungard(e, env));

	varval = escm_atom_eval(e, varcons->car);
	if (!varval) {
	    if (e->err != -1) {
		escm_atom_display(e, varcons->car, stderr);
		fprintf(stderr, ": expression not allowed in this context.\n");
		e->err = -1;
	    }
	    escm_gc_ungard(e, env);
	    return NULL;
	}

	escm_env_set(env, escm_sym_val(varname), varval);
    }

    /* we now enter in the new environment, eval the body and return the last
       result */
    escm_gc_ungard(e, env);
    prevenv = escm_env_enter(e, env);
    ret = NULL;
    for (arg = escm_cons_pop(e, &args); arg; arg = escm_cons_pop(e, &args)) {
	ret = escm_atom_eval(e, arg);
	if (!ret && e->err == -1) {
	    escm_env_leave(e, prevenv);
	    return NULL;
	}
    }

    escm_env_leave(e, prevenv);

    return ret;
}

escm_atom *
escm_let_star(escm *e, escm_atom *args)
{
    escm_atom *prevenv, *arg, *varname, *varval, *ret;
    escm_cons *c, *varcons;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    /* we first create a new empty environment, and we enter in */
    prevenv = escm_env_enter(e, escm_env_new(e, e->env));

    /* then we bind each variable in this new environment */
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
	escm_assert1(ESCM_ISCONS(c->car), c->car, e,
		     escm_env_leave(e, prevenv));

	varcons = escm_cons_val(c->car);
	escm_assert1(varcons, c->car, e, escm_env_leave(e, prevenv));

	varname = varcons->car, varcons = escm_cons_next(varcons);
	escm_assert1(ESCM_ISSYM(varname), c->car, e,
		     escm_env_leave(e, prevenv));
	escm_assert1(varcons != NULL && varcons->cdr == e->NIL, c->car, e,
		     escm_env_leave(e, prevenv));

	varval = escm_atom_eval(e, varcons->car);
	if (!varval) {
	    if (e->err != -1) {
		escm_atom_display(e, varcons->car, stderr);
		fprintf(stderr, ": expression not allowed in this context.\n");
		e->err = -1;
	    }
	    escm_env_leave(e, prevenv);
	    return NULL;
	}

	escm_env_set(e->env, escm_sym_val(varname), varval);
    }

    /* we now eval the body */
    ret = NULL;
    for (arg = escm_cons_pop(e, &args); arg; arg = escm_cons_pop(e, &args)) {
	ret = escm_atom_eval(e, arg);
	if (!ret && e->err == -1) {
	    escm_env_leave(e, prevenv);
	    return NULL;
	}
    }

    escm_env_leave(e, prevenv);

    return ret;
}

escm_atom *
escm_letrec(escm *e, escm_atom *args)
{
    escm_atom *prevenv, *arg, *varname, *ret;
    escm_cons *c, *varcons;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    /* we first create a new empty environment, and we enter in */
    prevenv = escm_env_enter(e, escm_env_new(e, e->env));

    /* we set the variables to NULL and the verify the validity of the
       formals */
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
	escm_assert1(ESCM_ISCONS(c->car), c->car, e,
		     escm_env_leave(e, prevenv));

	varcons = escm_cons_val(c->car);
	escm_assert1(varcons, c->car, e, escm_env_leave(e, prevenv));

	varname = varcons->car, varcons = escm_cons_next(varcons);
	escm_assert1(ESCM_ISSYM(varname), c->car, e,
		     escm_env_leave(e, prevenv));
	escm_assert1(varcons != NULL && varcons->cdr == e->NIL, c->car, e,
		     escm_env_leave(e, prevenv));

	escm_env_set(e->env, escm_sym_val(varname), NULL);
    }

    /* compute the values */
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
	varcons = escm_cons_val(c->car);
	varcons = escm_cons_next(varcons);

	ret = escm_atom_eval(e, varcons->car);
	if (!ret) {
	    if (e->err != -1) {
		escm_atom_display(e, varcons->car, stderr);
		fprintf(stderr, ": expression not allowed in this context.\n");
		e->err = -1;
	    }
	    escm_env_leave(e, prevenv);
	    return NULL;
	}
	varcons->car = ret;
    }

    /* and bind the variables */
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
	varcons = escm_cons_val(c->car);
	varname = varcons->car, varcons = escm_cons_next(varcons);

	escm_env_set(e->env, escm_sym_val(varname), varcons->car);
    }

    /* we now eval the body */
    ret = NULL;
    for (arg = escm_cons_pop(e, &args); arg; arg = escm_cons_pop(e, &args)) {
	ret = escm_atom_eval(e, arg);
	if (!ret && e->err == -1) {
	    escm_env_leave(e, prevenv);
	    return NULL;
	}
    }

    escm_env_leave(e, prevenv);

    return ret;
}

escm_atom *
escm_if(escm *e, escm_atom *args)
{
    escm_atom *test;
    escm_atom *a;

    a = escm_cons_pop(e, &args);

    test = escm_atom_eval(e, a);
    if (!test) {
	if (e->err != -1) {
	    escm_atom_display(e, a, stderr);
	    fprintf(stderr, ": expression not allowed in this context.\n");
	    e->err = -1;
	}
	return NULL;
    }

    if (ESCM_ISTRUE(test))
	return escm_atom_eval(e, escm_cons_pop(e, &args));
    else {
	(void) escm_cons_pop(e, &args); /* skip the 'true' statement */
	a = escm_cons_pop(e, &args);
	if (a)
	    return escm_atom_eval(e, a);
    }

    return NULL;
}

escm_atom *
escm_cond(escm *e, escm_atom *args)
{
    escm_cons *c;
    escm_atom *ret, *test;
    escm_atom *clause;

    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
	clause = c->car;
	escm_assert(ESCM_ISCONS(clause) && clause != e->NIL, clause, e);
	test = escm_cons_pop(e, &clause);

	if (ESCM_ISSYM(test) && 0 == strcmp("else", escm_sym_val(test)))
	    return escm_begin(e, clause);

	ret = escm_atom_eval(e, test);
	if (!ret) {
	    if (e->err != -1) {
		escm_atom_display(e, test, stderr);
		fprintf(stderr, ": expression not allowed in this context.\n");
		e->err = -1;
	    }
	    return NULL;
	}

	if (ESCM_ISTRUE(ret)) {
	    if (clause != NULL && ESCM_ISSYM(escm_cons_val(clause)->car) &&
		0 == strcmp("=>", escm_sym_val(escm_cons_val(clause)->car))) {
		escm_atom *proc;

		(void) escm_cons_pop(e, &clause);
		escm_assert(clause != NULL, c->car, e);
		proc = escm_atom_eval(e, escm_cons_pop(e, &clause));
		if (!proc)
		    return NULL;
		if (!ESCM_ISPROC(proc)) {
		    escm_atom_display(e, proc, stderr);
		    fprintf(stderr, ": procedure expected.\n");
		    e->err = -1;
		    return NULL;
		}
		return escm_procedure_exec(e, proc,
					   escm_cons_new(e, ret, e->NIL), 0);
	    } else
		return escm_begin(e, clause);
	}
    }

    return NULL;
}

escm_atom *
escm_case(escm *e, escm_atom *args)
{
    escm_cons *list;
    escm_atom *expr, *d, *clause;

    d = escm_cons_pop(e, &args);
    expr = escm_atom_eval(e, d);
    if (!expr) {
	if (e->err != -1) {
	    escm_atom_display(e, d, stderr);
	    fprintf(stderr, ": expression not allowed in this context.\n");
	}
	e->err = -1;
	return NULL;
    }

    for (clause = escm_cons_pop(e, &args); clause;
	 clause = escm_cons_pop(e, &args)) {
	escm_assert(ESCM_ISCONS(clause) && clause != e->NIL, clause, e);

	d = escm_cons_pop(e, &clause);
	escm_assert(ESCM_ISCONS(d) || ESCM_ISSYM(d), d, e);

	if (ESCM_ISSYM(d) && 0 == strcmp("else", escm_sym_val(d)))
	    return escm_begin(e, clause);

	for (list = escm_cons_val(d); list; list = escm_cons_next(list)) {
	    if (escm_atom_equal(e, list->car, expr, 0))
		return escm_begin(e, clause);
	}
    }

    return NULL;
}

escm_atom *
escm_and(escm *e, escm_atom *args)
{
    escm_atom *c, *ret;

    c = escm_cons_pop(e, &args);
    if (!c)
	return e->TRUE;

    ret = NULL; /* make splint happy */
    while (c) {
	ret = escm_atom_eval(e, c);
	if (!ret && !ESCM_ISTRUE(ret))
	    return ret;

	c = escm_cons_pop(e, &args);
    }

    return ret;
}

escm_atom *
escm_or(escm *e, escm_atom *args)
{
    escm_atom *c, *ret;

    c = escm_cons_pop(e, &args);
    if (!c)
	return e->FALSE;

    ret = NULL; /* make splint happy */
    while (c) {
	ret = escm_atom_eval(e, c);
	if (!ret || ESCM_ISTRUE(ret))
	    return ret; /* true value (or null) */

	c = escm_cons_pop(e, &args);
    }

    return ret;
}

escm_atom *
escm_begin(escm *e, escm_atom *args)
{
    escm_atom *a, *ret;
    if (!args) {
	fprintf(stderr, "begin: no arguments given.\n");
	e->err = -1;
	return NULL;
    }

    ret = NULL;
    for (a = escm_cons_pop(e, &args); a; a = escm_cons_pop(e, &args)) {
	ret = escm_atom_eval(e, a);
	if (!ret && e->err == -1)
	    return ret;
    }

    return ret;
}


escm_atom *
escm_do(escm *e, escm_atom *args)
{
    escm_cons *c, *cval;
    escm_atom *env, *prevenv, *test, *ret;
    escm_atom *clauses, *atom, *var, *varval;

    clauses = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(clauses), clauses, e);

    test = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(test) && test != e->NIL, test, e);

    env = escm_env_new(e, e->env);
    escm_gc_gard(e, env);

    /* eval the init and bound the vars */
    for (c = escm_cons_val(clauses); c; c = escm_cons_next(c)) {
	atom = c->car;
	escm_assert1(ESCM_ISCONS(atom) && atom != e->NIL, c->car, e,
		     escm_gc_ungard(e, env));

	var = escm_cons_pop(e, &atom);
	escm_assert1(var && ESCM_ISSYM(var), c->car, e, escm_gc_ungard(e, env));

	varval = escm_cons_pop(e, &atom);
	escm_assert1(varval, c->car, e, escm_gc_ungard(e, env));

	if (atom) { /* assert there is nothing after the step */
	    (void) escm_cons_pop(e, &atom);
	    escm_assert1(atom == NULL, c->car, e, escm_gc_ungard(e, env));
	}

	atom = varval;
	varval = escm_atom_eval(e, varval);
	if (!varval) {
	    if (e->err != -1) {
		escm_atom_display(e, atom, stderr);
		fprintf(stderr, ": expression not allowed in this context.\n");
		e->err = -1;
	    }
	    escm_gc_ungard(e, env);
	    return NULL;
	}

	escm_env_set(env, escm_sym_val(var), varval);    
    }

    escm_gc_ungard(e, env);
    prevenv = escm_env_enter(e, env);

    /* now iterate */
    for (;;) {
	atom = escm_atom_eval(e, escm_cons_val(test)->car);
	if (ESCM_ISTRUE(atom)) {
	    if (escm_cons_val(test)->cdr != e->NIL)
		ret = escm_begin(e, escm_cons_val(test)->cdr);
	    else
		ret = NULL;
	    goto end;
	} else {
	    /* execute command */
	    if (args)
		escm_begin(e, args);

	    /* run steps and rebound vars */
	    escm_ctx_enter(e);

	    for (c = escm_cons_val(clauses); c; c = escm_cons_next(c)) {
		atom = c->car;
		var = escm_cons_pop(e, &atom);
		(void) escm_cons_pop(e, &atom); /* init */
		if (atom) { /* if there is a step */
		    varval = escm_atom_eval(e, escm_cons_pop(e, &atom));
		    if (!varval) {
			if (e->err != -1) {
			    escm_atom_display(e, atom, stderr);
			    fprintf(stderr, ": expression not allowed in this "
				    "context.\n");
			    e->err = -1;
			}
			escm_ctx_discard(e);
			ret = NULL;
			goto end;
		    }
		} else
		    varval = escm_atom_eval(e, var);
		escm_ctx_put(e, varval);
	    }

	    cval = escm_cons_val(escm_ctx_leave(e));
	    for (c = escm_cons_val(clauses); c;
		 c = escm_cons_next(c), cval = escm_cons_next(cval)) {
		atom = c->car;
		var = escm_cons_pop(e, &atom);

		escm_env_set(e->env, escm_sym_val(var), cval->car);
	    }
	}
    }

end:
    escm_env_leave(e, prevenv);
    return ret;
}

escm_atom *
escm_eq_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;

    a1 = escm_cons_pop(e, &args);
    a2 = escm_cons_pop(e, &args);

    return (escm_atom_equal(e, a1, a2, 0)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_eqv_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;

    a1 = escm_cons_pop(e, &args);
    a2 = escm_cons_pop(e, &args);

    return (escm_atom_equal(e, a1, a2, 1)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_equal_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;

    a1 = escm_cons_pop(e, &args);
    a2 = escm_cons_pop(e, &args);

    return (escm_atom_equal(e, a1, a2, 2)) ? e->TRUE : e->FALSE;
}

static escm_atom *
named_let(escm *e, escm_atom *name, escm_atom *args)
{
    escm_atom *bindings, *val;
    escm_atom *fun, *prevenv;
    escm_cons *c, *cons;

    bindings = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(bindings), bindings, e);
    if (!args) {
	fprintf(stderr, "missing body.\n");
	e->err = -1;
	return NULL;
    }
    
    escm_ctx_enter(e); /* the context of the lambda construction */

    /* first we create a list of the values */
    escm_ctx_enter(e);

    for (c = escm_cons_val(bindings); c; c = escm_cons_next(c)) {
	escm_assert1(ESCM_ISCONS(c->car), c->car, e,
		     escm_ctx_discard(e); escm_ctx_discard(e));

	cons = escm_cons_val(c->car);
	escm_assert1(cons != NULL && ESCM_ISSYM(cons->car), c->car, e,
		     escm_ctx_discard(e); escm_ctx_discard(e));

	val = cons->cdr;
	escm_assert1(val != e->NIL && ESCM_ISCONS(val) &&
		     escm_cons_val(val)->cdr == e->NIL, c->car,
		     e, escm_ctx_discard(e); escm_ctx_discard(e));


	escm_ctx_put(e, cons->car);
    }

    escm_ctx_put(e, escm_ctx_leave(e)); /* add the formals */
    escm_ctx_put(e, escm_cons_pop(e, &args)); /* the body */

    prevenv = escm_env_enter(e, escm_env_new(e, e->env));

    fun = escm_procedure_exec(e, e->LAMBDA, escm_ctx_leave(e), 0);
    if (!fun) {
	escm_env_leave(e, prevenv);
	return NULL;
    }
    /* bind the function to its name */
    escm_env_set(e->env, escm_sym_val(name), fun);

    escm_ctx_enter(e); /* now we create a list of the arguments */

    for (c = escm_cons_val(bindings); c; c = escm_cons_next(c)) {
	cons = escm_cons_val(escm_cons_val(c->car)->cdr);

	escm_ctx_put(e, cons->car);
    }

    val = escm_procedure_exec(e, fun, escm_ctx_leave(e), 1);
    escm_env_leave(e, prevenv);

    return val;
}
