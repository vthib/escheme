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

static escm_atom *named_let(escm *, escm_atom *, escm_atom *);
static escm_atom *quasiquote(escm *, escm_atom *, unsigned int);
#ifdef ESCM_USE_VECTORS
static escm_atom *quasiquote_vector(escm *, escm_atom *, unsigned int);
#endif

void
escm_primitives_load(escm *e)
{
    escm_atom *o;

    o = escm_procedure_new(e, "quote", 1, 1, escm_quote, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "quasiquote", 1, 1, escm_quasiquote, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, "lambda", 2, -1, escm_lambda, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, "define", 2, -1, escm_define, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "set!", 2, 2, escm_set_x, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, "let", 2, -1, escm_let, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, "let*", 2, -1, escm_let_star, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, "letrec", 2, -1, escm_letrec, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, "if", 2, 3, escm_if, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, "cond", 1, -1, escm_cond, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "case", 2, -1, escm_case, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, "and", 0, -1, escm_and, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "or", 0, -1, escm_or, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, "begin", 1, -1, escm_begin, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "do", 2, -1, escm_do, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    (void) escm_procedure_new(e, "eqv?", 2, 2, escm_eqv_p, NULL);
    (void) escm_procedure_new(e, "eq?", 2, 2, escm_eq_p, NULL);
    (void) escm_procedure_new(e, "equal?", 2, 2, escm_equal_p, NULL);

    (void) escm_procedure_new(e, "eof-object?", 1, 1, escm_eof_object_p, NULL);

    (void) escm_procedure_new(e, "load", 1, 1, escm_load, NULL);

#ifndef ESCM_USE_PORTS
    (void) escm_procedure_new(e, "read", 0, 1, escm_read, NULL);
# ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, "read-char", 0, 1, escm_read_char, NULL);
    (void) escm_procedure_new(e, "peek-char", 0, 1, escm_peek_char, NULL);
# endif

    (void) escm_procedure_new(e, "write", 1, 2, escm_write, NULL);
    (void) escm_procedure_new(e, "display", 1, 2, escm_display, NULL);
    (void) escm_procedure_new(e, "newline", 0, 1, escm_newline, NULL);
# ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, "write-char", 1, 2, escm_write_char, NULL);
# endif
#endif

    (void) escm_procedure_new(e, "gc", 0, 0, escm_gc, NULL);
    (void) escm_procedure_new(e, "set-case-sensitive!", 1, 1,
			      escm_set_case_sensitive_x, NULL);
    (void) escm_procedure_new(e, "set-brackets-parens!", 1, 1,
			      escm_set_brackets_parens_x, NULL);
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
    escm_atom *arg;

    arg = escm_cons_pop(e, &args);
    if (!ESCM_ISCONS(arg)
#ifdef ESCM_USE_VECTORS
	& !ESCM_ISVECTOR(arg)) {
#endif
	arg->ro = 1;
	return arg;
    }

    return quasiquote(e, arg, 1);
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
	while (args)
	    escm_ctx_put(e, escm_cons_pop(e, &args)); /* body */

	escm_env_set(e->env, escm_sym_val(a->car),
		     escm_lambda(e, escm_ctx_first(e)));
	escm_ctx_discard(e);
    } else {
	escm_atom *val;

	if (escm_cons_cdr(args) != e->NIL) {
	    fprintf(stderr, "define: error: multiples expressions.\n");
	    e->err = -1;
	    return NULL;
	}

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
	escm_atom_print(e, c, stderr);
	fprintf(stderr, ": unknown identifier.\n");
	e->err = -1;
	return NULL;
    }

    escm_env_edit(e->env, escm_sym_val(c), escm_cons_pop(e, &args));

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
		escm_atom_print(e, varcons->car, stderr);
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
		escm_atom_print(e, varcons->car, stderr);
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
		escm_atom_print(e, varcons->car, stderr);
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
	    escm_atom_print(e, a, stderr);
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
		escm_atom_print(e, test, stderr);
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
		    escm_atom_print(e, proc, stderr);
		    fprintf(stderr, ": procedure expected.\n");
		    e->err = -1;
		    return NULL;
		}
		return escm_procedure_exec(e, proc,
					   escm_cons_make(e, ret, e->NIL), 0);
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
	    escm_atom_print(e, d, stderr);
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
	if (!ret || !ESCM_ISTRUE(ret))
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
	escm_assert1(var != NULL && ESCM_ISSYM(var), c->car, e,
		     escm_gc_ungard(e, env));

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
		escm_atom_print(e, atom, stderr);
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

    ret = NULL;
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
		(void) escm_begin(e, args);

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
			    escm_atom_print(e, atom, stderr);
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

escm_atom *
escm_eof_object_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    return (a == e->EOF_OBJ) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_load(escm *e, escm_atom *args)
{
    escm_atom *str;
    escm_context *ctx;
    unsigned int save;

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    save = e->quiet, e->quiet = 1;
    ctx = e->ctx, e->ctx = NULL;
    escm_fparse(e, escm_str_val(str));
    e->quiet = save, e->ctx = ctx;

    return NULL;
}

/* if we don't use ports, we implement those functions to work only with
   stdin and stdout */
#ifndef ESCM_USE_PORTS
escm_atom *
escm_read(escm *e, escm_atom *args)
{
    escm_atom *a;
    escm_input *save;

    (void) args;

    save = e->input;
    e->input = escm_input_fmng(stdin, "standard input");
    a = escm_parse(e);
    escm_input_close(e->input), e->input = save;

    return a;
}

# ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_read_char(escm *e, escm_atom *args)
{
    int c;

    (void) args;

    c = getchar();
    if (c == EOF)
	return e->EOF_OBJ;

    return escm_char_make(e, c);
}

escm_atom *
escm_peek_char(escm *e, escm_atom *args)
{
    int c;

    (void) args;

    c = getchar();
    if (c == EOF)
	return e->EOF_OBJ;

    if (EOF == ungetc(c, stdin))
	fprintf(stderr, "ungetc failed.\n");

    return escm_char_make(e, c);
}
# endif /* ESCM_USE_CHARACTERS */

escm_atom *
escm_write(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);

    escm_atom_print(e, atom, stdout);

    return NULL;
}

escm_atom *
escm_display(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);

    escm_atom_print0(e, atom, stdout, 1);

    return NULL;
}

escm_atom *
escm_newline(escm *e, escm_atom *args)
{
    (void) e;
    (void) args;

    printf("\n");

    return NULL;
}

# ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_write_char(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    putc(escm_char_val(c), stdout);

    return NULL;
}
# endif /* ESCM_USE_CHARACTERS */
#endif /* ESCM_USE_PORTS */

escm_atom *
escm_gc(escm *e, escm_atom *args)
{
    (void) args;

    escm_gc_collect(e);

    return NULL;
}

escm_atom *
escm_set_case_sensitive_x(escm *e, escm_atom *args)
{
    e->casesensitive = ESCM_ISTRUE(escm_cons_car(args));

    return NULL;
}

escm_atom *
escm_set_brackets_parens_x(escm *e, escm_atom *args)
{
    e->brackets = ESCM_ISTRUE(escm_cons_car(args));

    return NULL;
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

    fun = escm_lambda(e, escm_ctx_leave(e));
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

static escm_atom *
quasiquote(escm *e, escm_atom *atom, unsigned int lvl)
{
    escm_atom *ret;
    escm_cons *c;

    if (lvl == 0)
	return escm_atom_eval(e, atom);

#ifdef ESCM_USE_VECTORS
	if (ESCM_ISVECTOR(atom))
	    return quasiquote_vector(e, atom, lvl);
#endif

    if (!ESCM_ISCONS(atom))
	return atom;

    escm_ctx_enter(e);

    for (c = escm_cons_val(atom); c; c = escm_cons_next(c)) {
	if (!ESCM_ISCONS(c->cdr)) {
	    e->ctx->dotted = 1;
	    escm_ctx_put(e, c->cdr);
	} else if (ESCM_ISSYM(c->car)) {
	    /* this form can be found when adding a unquote after a dot
	       (ie `(1 2 . ,a) -> `(1 2 unquote a) */
	    if (0 == strcmp(escm_sym_val(c->car), "unquote")) {
		if (c->cdr == e->NIL) {
		    fprintf(stderr, "unquote expect exactly one argument.\n");
		    goto err;
		}
		ret = quasiquote(e, escm_cons_val(c->cdr)->car, lvl - 1);
		if (!ret)
		    goto err;
		e->ctx->dotted = 1;
		escm_ctx_put(e, ret);
		break;
	    } else if (0 == strcmp(escm_sym_val(c->car), "unquote-splicing")) {
		fprintf(stderr, "unquote-splicing found after a dotted "
			"notation.\n");
		goto err;
	    } else
		escm_ctx_put(e, c->car);
	} else if (ESCM_ISCONS(c->car)) {
	    if (ESCM_ISSYM(escm_cons_val(c->car)->car)) {
		escm_cons *cons;

		cons = escm_cons_val(c->car);
		if (0 == strcmp(escm_sym_val(cons->car), "unquote")) {
		    if (cons->cdr == e->NIL) {
			fprintf(stderr, "unquote expect exactly one "
				"argument.\n");
			goto err;
		    }
		    if (lvl != 1) {
			escm_ctx_enter(e);
			escm_ctx_put(e, cons->car);
		    }
		    ret = quasiquote(e, escm_cons_val(cons->cdr)->car, lvl - 1);
		    if (!ret)
			goto err;
		    escm_ctx_put(e, ret);
		    if (lvl != 1)
			escm_ctx_put(e, escm_ctx_leave(e));
		} else if (0 == strcmp(escm_sym_val(cons->car),
				       "unquote-splicing")) {
		    if (cons->cdr == e->NIL) {
			fprintf(stderr, "unquote-splicing expect exactly one "
				"argument.\n");
			goto err;
		    }
		    if (lvl != 1) {
			escm_ctx_enter(e);
			escm_ctx_put(e, cons->car);
		    }
		    ret = quasiquote(e, escm_cons_val(cons->cdr)->car, lvl - 1);
		    if (!ret)
			goto err;
		    else if (!ESCM_ISCONS(ret)) {
			fprintf(stderr, "unquote-splicing expect a argument of "
				"type <list>.\n");
			goto err;
		    }
		    if (lvl != 1) {
			escm_ctx_put(e, ret);
			escm_ctx_put(e, escm_ctx_leave(e));
		    } else
			escm_ctx_put_splicing(e, ret);
		} else if (0 == strcmp(escm_sym_val(cons->car), "quasiquote")
		    && cons->cdr != e->NIL) {
		    escm_ctx_enter(e);
		    escm_ctx_put(e, cons->car);
		    ret = quasiquote(e, escm_cons_val(cons->cdr)->car, lvl + 1);
		    if (!ret)
			goto err;
		    escm_ctx_put(e, ret);
		    escm_ctx_put(e, escm_ctx_leave(e));
		} else {
		    ret = quasiquote(e, c->car, lvl);
		    if (!ret)
			goto err;
		    escm_ctx_put(e, ret);
		}
	    }
	} else
	    escm_ctx_put(e, c->car);
    }

    return escm_ctx_leave(e);

err:
    escm_ctx_discard(e);
    return NULL;
}

#ifdef ESCM_USE_VECTORS
static escm_atom *
quasiquote_vector(escm *e, escm_atom *atom, unsigned int lvl)
{
    escm_atom *ret;
    escm_vector *v;
    size_t i;

    escm_ctx_enter(e);

    v = escm_vector_val(atom);
    for (i = 0; i < v->len; i++) {
	if (ESCM_ISCONS(v->vec[i])) {
	    if (ESCM_ISSYM(escm_cons_val(v->vec[i])->car)) {
		escm_cons *cons;

		cons = escm_cons_val(v->vec[i]);
		if (0 == strcmp(escm_sym_val(cons->car), "unquote")) {
		    if (cons->cdr == e->NIL) {
			fprintf(stderr, "unquote expect exactly one "
				"argument.\n");
			goto err;
		    }
		    if (lvl != 1) {
			escm_ctx_enter(e);
			escm_ctx_put(e, cons->car);
		    }
		    ret = quasiquote(e, escm_cons_val(cons->cdr)->car, lvl - 1);
		    if (!ret)
			goto err;
		    escm_ctx_put(e, ret);
		    if (lvl != 1)
			escm_ctx_put(e, escm_ctx_leave(e));
		} else if (0 == strcmp(escm_sym_val(cons->car),
				       "unquote-splicing")) {
		    if (cons->cdr == e->NIL) {
			fprintf(stderr, "unquote-splicing expect exactly one "
				"argument.\n");
			goto err;
		    }
		    if (lvl != 1) {
			escm_ctx_enter(e);
			escm_ctx_put(e, cons->car);
		    }
		    ret = quasiquote(e, escm_cons_val(cons->cdr)->car, lvl - 1);
		    if (!ret)
			goto err;
		    else if (!ESCM_ISCONS(ret)) {
			fprintf(stderr, "unquote-splicing expect a argument of "
				"type <list>.\n");
			goto err;
		    }
		    if (lvl != 1) {
			escm_ctx_put(e, ret);
			escm_ctx_put(e, escm_ctx_leave(e));
		    } else
			escm_ctx_put_splicing(e, ret);
		} else if (0 == strcmp(escm_sym_val(cons->car), "quasiquote")
		    && cons->cdr != e->NIL) {
		    escm_ctx_enter(e);
		    escm_ctx_put(e, cons->car);
		    ret = quasiquote(e, escm_cons_val(cons->cdr)->car, lvl + 1);
		    if (!ret)
			goto err;
		    escm_ctx_put(e, ret);
		    escm_ctx_put(e, escm_ctx_leave(e));
		} else {
		    ret = quasiquote(e, v->vec[i], lvl);
		    if (!ret)
			goto err;
		    escm_ctx_put(e, ret);
		}
	    }
	} else
	    escm_ctx_put(e, v->vec[i]);
    }

    return escm_prim_vector(e, escm_ctx_leave(e));

err:
    escm_ctx_discard(e);
    return NULL;
}
#endif
