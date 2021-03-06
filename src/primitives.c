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
#include <string.h>

#include "primitives.h"
#include "base.h"
#include "type/strings.h"
#include "type/chars.h"
#include "type/vectors.h"
#include "type/ports.h"

struct list {
    escm_atom *atom;
    struct list *next;
};

struct e_args {
	escm *e;
	escm_atom *args;
};

static escm_atom *named_let(escm *, escm_atom *, escm_atom *);
static escm_atom *quasiquote(escm *, escm_atom *, unsigned int);
#ifdef ESCM_USE_VECTORS
static escm_atom *quasiquote_vector(escm *, escm_atom *, unsigned int);
#endif
static escm_atom *next_arg(struct e_args *);

void
escm_primitives_load(escm *e)
{
    escm_atom *o;

    o = escm_procedure_new(e, T("quote"), 1, 1, escm_quote, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, T("quasiquote"), 1, 1, escm_quasiquote, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, T("lambda"), 2, -1, escm_lambda, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, T("define"), 2, -1, escm_define, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, T("set!"), 2, 2, escm_set_x, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, T("let"), 2, -1, escm_let, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, T("let*"), 2, -1, escm_let_star, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, T("letrec"), 2, -1, escm_letrec, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, T("if"), 2, 3, escm_if, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, T("cond"), 1, -1, escm_cond, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, T("case"), 2, -1, escm_case, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    o = escm_procedure_new(e, T("and"), 0, -1, escm_and, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, T("or"), 0, -1, escm_or, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, T("do"), 2, -1, escm_do, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;

    (void) escm_procedure_new(e, T("eqv?"), 2, 2, escm_eqv_p, NULL);
    (void) escm_procedure_new(e, T("eq?"), 2, 2, escm_eq_p, NULL);
    (void) escm_procedure_new(e, T("equal?"), 2, 2, escm_equal_p, NULL);

    (void) escm_procedure_new(e, T("eof-object?"), 1, 1, escm_eof_object_p, NULL);

#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, T("load"), 1, 1, escm_load, NULL);
#endif

#ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, T("read-char"), 0, 1, escm_read_char, NULL);
    (void) escm_procedure_new(e, T("peek-char"), 0, 1, escm_peek_char, NULL);
    (void) escm_procedure_new(e, T("write-char"), 1, 2, escm_write_char, NULL);
    (void) escm_procedure_new(e, T("unread-char"), 1, 2, escm_unread_char, NULL);
#endif
    (void) escm_procedure_new(e, T("read"), 0, 1, escm_read, NULL);
    (void) escm_procedure_new(e, T("write"), 1, 2, escm_write, NULL);
    (void) escm_procedure_new(e, T("display"), 1, 2, escm_display, NULL);
    (void) escm_procedure_new(e, T("newline"), 0, 1, escm_newline, NULL);

    (void) escm_procedure_new(e, T("gc"), 0, 0, escm_gc, NULL);
    (void) escm_procedure_new(e, T("backtrace"), 0, 0, escm_backtrace, NULL);

    (void) escm_procedure_new(e, T("read-only!"), 1, 1, escm_read_only_x, NULL);
    (void) escm_procedure_new(e, T("read-only?"), 1, 1, escm_read_only_p, NULL);

    o = escm_procedure_new(e, T("assert"), 1, 1, escm_prim_assert, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    (void) escm_procedure_new(e, T("set-case-sensitive!"), 1, 1,
                              escm_set_case_sensitive_x, NULL);
    (void) escm_procedure_new(e, T("set-brackets-parens!"), 1, 1,
                              escm_set_brackets_parens_x, NULL);
    (void) escm_procedure_new(e, T("set-print-backtrace!"), 1, 1,
                              escm_set_print_backtrace_x, NULL);

    (void) escm_procedure_new(e, T("exit"), 0, 0, escm_exit, NULL);
    (void) escm_procedure_new(e, T("test"), 3, 3, escm_test, NULL);
    o = escm_procedure_new(e, T("test-error"), 2, 2, escm_test_error, NULL);
    escm_proc_val(o)->d.c.quoted = 0x2;

#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, T("printf"), 1, -1, escm_prim_printf, NULL);
    (void) escm_procedure_new(e, T("format"), 1, -1, escm_format, NULL);
#endif
}

escm_atom *
escm_quote(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) nil;

    escm_cons_val(args)->car->ro = 1;

    return escm_cons_val(args)->car;
}

escm_atom *
escm_quasiquote(escm *e, escm_atom *args, void *nil)
{
    escm_atom *arg;

    (void) nil;
    arg = escm_cons_pop(e, &args);
    if (!ESCM_ISCONS(arg)
#ifdef ESCM_USE_VECTORS
        && !ESCM_ISVECTOR(arg)
#endif
        ) {
        arg->ro = 1;
        return arg;
    }

    return quasiquote(e, arg, 1);
}

escm_atom *
escm_lambda(escm *e, escm_atom *args, void *nil)
{
    escm_atom *params;
    escm_procedure *lambda;

    (void) nil;
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
escm_define(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);

    escm_assert(ESCM_ISSYM(c) || ESCM_ISCONS(c), c, e);

    if (ESCM_ISCONS(c)) {
        escm_cons *a;
        escm_atom *proc;

        a = escm_cons_val(c);
        escm_assert(a != NULL && ESCM_ISSYM(a->car), c, e);

        escm_ctx_enter(e);
        escm_ctx_put(e, a->cdr); /* formals */
        while (args != e->NIL)
            escm_ctx_put(e, escm_cons_pop(e, &args)); /* body */

        proc = escm_lambda(e, escm_ctx_first(e), NULL);
        if (!escm_proc_val(proc)->name)
            escm_proc_val(proc)->name = tcsdup(escm_sym_name(a->car));
        escm_env_set(e, e->env, a->car, proc);
        escm_ctx_discard(e);
    } else {
        escm_atom *val;

        if (escm_cons_cdr(args) != e->NIL) {
            escm_error(e, _(T("~s: error: multiples expressions.~%")),
                       escm_fun(e));
            escm_abort(e);
        }

        escm_gc_gard(e, c);
        val = escm_atom_eval(e, escm_cons_pop(e, &args));
        escm_gc_ungard(e, c);
        if (!val)
            return NULL;

        if (ESCM_ISCLOSURE(val) && !escm_proc_val(val)->name)
            escm_proc_val(val)->name = tcsdup(escm_sym_name(c));
        escm_env_set(e, e->env, c, val);
    }

    return NULL;
}

escm_atom *
escm_set_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);

    escm_assert(ESCM_ISSYM(c), c, e);

    if (escm_sym_val(c) == NULL) {
        escm_error(e, _(T("~s: unknown identifier: ~s.~n")), escm_fun(e), c);
        return NULL;
    }

    escm_symbol_set(c, escm_cons_pop(e, &args));

    return NULL;
}

escm_atom *
escm_let(escm *e, escm_atom *args, void *nil)
{
    escm_atom *env, *prevenv, *arg, *varname, *varval, *ret;
    escm_cons *c, *varcons;

    (void) nil;
    arg = escm_cons_pop(e, &args);
    if (ESCM_ISSYM(arg))
        return named_let(e, arg, args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

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
            if (e->err != 1)
                escm_error(e, _(T("~s: ~s: expression not allowed in this "))
                           _(T("context.~%")), escm_fun(e), varcons->car);
            escm_gc_ungard(e, env);
            escm_abort(e);
        }

        if (ESCM_ISCLOSURE(varval) && !escm_proc_val(varval)->name)
            escm_proc_val(varval)->name = tcsdup(escm_sym_name(varname));
        escm_env_set(e, env, varname, varval);
    }

    /* we now enter in the new environment, eval the body and return the last
       result */
    escm_gc_ungard(e, env);
    prevenv = escm_env_enter(e, env);
    ret = escm_begin(e, args, 1);

    escm_env_leave(e, prevenv);

    return ret;
}

escm_atom *
escm_let_star(escm *e, escm_atom *args, void *nil)
{
    escm_atom *prevenv, *arg, *varname, *varval, *ret;
    escm_cons *c, *varcons;

    (void) nil;
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
            if (e->err != 1)
                escm_error(e, _(T("~s: ~s: expression not allowed in this "))
                           _(T("context.~%")), escm_fun(e), varcons->car);
            escm_env_leave(e, prevenv);
            escm_abort(e);
        }

        if (ESCM_ISCLOSURE(varval) && !escm_proc_val(varval)->name)
            escm_proc_val(varval)->name = tcsdup(escm_sym_name(varname));
        escm_env_set(e, e->env, varname, varval);
    }

    /* we now eval the body */
    ret = escm_begin(e, args, 1);

    escm_env_leave(e, prevenv);

    return ret;
}

escm_atom *
escm_letrec(escm *e, escm_atom *args, void *nil)
{
    escm_atom *prevenv, *arg, *varname, *ret;
    escm_cons *c, *varcons;
    struct list *first, *last;

    (void) nil;
    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    /* we first create a new empty environment, and we enter in */
    prevenv = escm_env_enter(e, escm_env_new(e, e->env));

    first = NULL, last = NULL;

    /* we set the variables to NULL and verify the validity of the formals */
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

        escm_env_set(e, e->env, varname, NULL);
    }

    /* compute the values */
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
        varcons = escm_cons_val(escm_cons_cdr(c->car));

        ret = escm_atom_eval(e, varcons->car);
        if (!ret) {
            if (e->err != 1)
                escm_error(e, _(T("~s: ~s: expression not allowed in "))
                           _(T("this context.~%")), escm_fun(e), varcons->car);
            escm_env_leave(e, prevenv);
            escm_ctx_discard(e);
            escm_abort(e);
        }

        /* we stock the results in a list */
        if (!first) {
            first = xcalloc(1, sizeof *first);
            first->atom = ret;
            last = first;
        } else {
            last->next = xcalloc(1, sizeof *last->next);
            last->next->atom = ret;
            last = last->next;
        }
    }

    /* and bind the variables */
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
        varname = escm_cons_car(c->car);

        ret = first->atom;
        last = first->next, free(first);
        first = last;

        if (ESCM_ISCLOSURE(ret) && !escm_proc_val(ret)->name)
            escm_proc_val(ret)->name = tcsdup(escm_sym_name(varname));
        escm_symbol_set(varname, ret);
    }

    /* we now eval the body */
    ret = escm_begin(e, args, 1);

    escm_env_leave(e, prevenv);

    return ret;
}

escm_atom *
escm_if(escm *e, escm_atom *args, void *nil)
{
    escm_atom *test;
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);

    test = escm_atom_eval(e, a);
    if (e->err == 1)
        return NULL;

    e->ctx->tailrec = 1;

    if (ESCM_ISTRUE(e, test)) {
        a = escm_cons_pop(e, &args);
        if (!escm_tailrec(e, a))
            return NULL;
        return escm_atom_eval(e, a);
    } else {
        (void) escm_cons_pop(e, &args); /* skip the 'true' statement */
        a = escm_cons_pop(e, &args);
        if (a) {
            if (!escm_tailrec(e, a))
                return NULL;
            return escm_atom_eval(e, a);
        }
    }

    return NULL;
}

escm_atom *
escm_cond(escm *e, escm_atom *args, void *nil)
{
    escm_cons *c;
    escm_atom *ret, *test;
    escm_atom *clause;

    (void) nil;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
        clause = c->car;
        escm_assert(ESCM_ISCONS(clause) && clause != e->NIL, clause, e);
        test = escm_cons_pop(e, &clause);

        if (ESCM_ISSYM(test) && 0 == tcscmp(T("else"), escm_sym_name(test)))
            return escm_begin(e, clause, 1);

        ret = escm_atom_eval(e, test);
        if (!ret) {
            if (e->err != 1)
                escm_error(e, _(T("~s: ~s: expression not allowed in this "))
                           _(T("context.~%")), escm_fun(e), test);
            escm_abort(e);
        }

        if (ESCM_ISTRUE(e, ret)) {
            if (clause != e->NIL && ESCM_ISSYM(escm_cons_car(clause)) &&
                0 == tcscmp(T("=>"), escm_sym_name(escm_cons_car(clause)))) {
                escm_atom *proc;

                (void) escm_cons_pop(e, &clause);
                escm_assert(clause != NULL, c->car, e);
                proc = escm_atom_eval(e, escm_cons_pop(e, &clause));
                if (!proc)
                    return NULL;
                if (!ESCM_ISPROC(proc)) {
                    escm_error(e, _(T("~s: ~s: procedure expected.~%")),
                               escm_fun(e), proc);
                    escm_abort(e);
                }
                e->ctx->tailrec = 1;
                return escm_procedure_exec(e, proc,
                                           escm_cons_make(e, ret, e->NIL), 0);
            } else
                return escm_begin(e, clause, 1);
        }
    }

    return NULL;
}

escm_atom *
escm_case(escm *e, escm_atom *args, void *nil)
{
    escm_cons *list;
    escm_atom *expr, *d, *clause;

    (void) nil;
    d = escm_cons_pop(e, &args);
    expr = escm_atom_eval(e, d);
    if (!expr) {
        if (e->err != 1)
            escm_error(e, _(T("~s: ~s: expression not allowed in this "))
                       _(T("context.~%")), escm_fun(e), d);
        escm_abort(e);
    }

    for (clause = escm_cons_pop(e, &args); clause;
         clause = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISCONS(clause) && clause != e->NIL, clause, e);

        d = escm_cons_pop(e, &clause);
        escm_assert(ESCM_ISCONS(d) || ESCM_ISSYM(d), d, e);

        if (ESCM_ISSYM(d) && 0 == tcscmp(T("else"), escm_sym_name(d)))
            return escm_begin(e, clause, 1);

        for (list = escm_cons_val(d); list; list = escm_cons_next(list)) {
            if (escm_atom_equal(e, list->car, expr, 0))
                return escm_begin(e, clause, 1);
        }
    }

    return NULL;
}

escm_atom *
escm_and(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c, *ret;

    (void) nil;
    c = escm_cons_pop(e, &args);
    if (!c)
        return e->TRUE;

    ret = NULL; /* make splint happy */
    while (c) {
        if (args == e->NIL) {
            e->ctx->tailrec = 1;
            if (!escm_tailrec(e, c))
                return NULL;
        }
        ret = escm_atom_eval(e, c);
        if (e->err == 1)
            return NULL;
        if (!ESCM_ISTRUE(e, ret))
            return ret;

        c = escm_cons_pop(e, &args);
    }

    return ret;
}

escm_atom *
escm_or(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c, *ret;

    (void) nil;
    c = escm_cons_pop(e, &args);
    if (!c)
        return e->FALSE;

    ret = NULL; /* make splint happy */
    while (c) {
        if (args == e->NIL) {
            e->ctx->tailrec = 1;
            if (!escm_tailrec(e, c))
                return NULL;
        }
        ret = escm_atom_eval(e, c);
        if (e->err == 1)
            return NULL;
        if (!ret || ESCM_ISTRUE(e, ret))
            return ret; /* true value (or null) */

        c = escm_cons_pop(e, &args);
    }

    return ret;
}

escm_atom *
escm_do(escm *e, escm_atom *args, void *nil)
{
    escm_cons *c;
    escm_atom *env, *prevenv, *test, *ret;
    escm_atom *clauses, *atom, *var, *varval;

    (void) nil;
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

        if (atom != e->NIL) { /* assert there is nothing after the step */
            (void) escm_cons_pop(e, &atom);
            escm_assert1(atom == e->NIL, c->car, e, escm_gc_ungard(e, env));
        }

        atom = varval;
        varval = escm_atom_eval(e, varval);
        if (!varval) {
            if (e->err != 1)
                escm_error(e, _(T("~s: ~s: expression not allowed in this "))
                           _(T("context.~%")), escm_fun(e), atom);
            escm_gc_ungard(e, env);
            escm_abort(e);
        }

        escm_env_set(e, env, var, varval);
    }

    escm_gc_ungard(e, env);
    prevenv = escm_env_enter(e, env);

    ret = NULL;
    /* now iterate */
    for (;;) {
        atom = escm_atom_eval(e, escm_cons_val(test)->car);
        if (ESCM_ISTRUE(e, atom)) {
            if (escm_cons_val(test)->cdr != e->NIL)
                ret = escm_begin(e, escm_cons_val(test)->cdr, 1);
            else
                ret = NULL;
            goto end;
        } else {
            /* execute command */
            if (args != e->NIL)
                (void) escm_begin(e, args, 0);

            /* run steps and rebound vars */

            env = escm_env_new(e, prevenv);
            escm_gc_gard(e, env);

            for (c = escm_cons_val(clauses); c; c = escm_cons_next(c)) {
                atom = c->car;
                var = escm_cons_pop(e, &atom);
                (void) escm_cons_pop(e, &atom); /* init */
                if (atom != e->NIL) { /* if there is a step */
                    varval = escm_atom_eval(e, escm_cons_pop(e, &atom));
                    if (!varval) {
                        if (e->err != 1) {
                            escm_error(e, _(T("~s: ~s: expression not "))
                                       _(T("allowed in this context.~%")),
                                       escm_fun(e), atom);
                            e->err = 1;
                        }
                        escm_gc_ungard(e, env);
                        ret = NULL;
                        goto end;
                    }
                } else
                    varval = escm_atom_eval(e, var);

                escm_env_set(e, env, var, varval);
            }

            escm_env_leave(e, prevenv);
            prevenv = escm_env_enter(e, env);
        }
    }

end:
    escm_env_leave(e, prevenv);
    return ret;
}

escm_atom *
escm_eq_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a1, *a2;

    (void) nil;
    a1 = escm_cons_pop(e, &args);
    a2 = escm_cons_pop(e, &args);

    return (escm_atom_equal(e, a1, a2, 0)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_eqv_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a1, *a2;

    (void) nil;
    a1 = escm_cons_pop(e, &args);
    a2 = escm_cons_pop(e, &args);

    return (escm_atom_equal(e, a1, a2, 1)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_equal_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a1, *a2;

    (void) nil;
    a1 = escm_cons_pop(e, &args);
    a2 = escm_cons_pop(e, &args);

    return (escm_atom_equal(e, a1, a2, 2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_eof_object_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    return (a == e->EOF_OBJ) ? e->TRUE : e->FALSE;
}

#ifdef ESCM_USE_STRINGS
escm_atom *
escm_load(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;
    escm_context *ctx;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_STRING)) {
        escm_error(e, _(T("~s: string type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    ctx = e->ctx, e->ctx = NULL;
#ifdef ESCM_UNICODE
    {
        char *s;

        s = tcstostr(escm_str_val(str));
        if (!escm_fparse(e, s)) {
            free(s);
			goto err;
        }
        free(s);
    }
#else
    if (!escm_fparse(e, escm_str_val(str)))
        goto err;
#endif

    e->ctx = ctx;
    return NULL;

err:
	e->ctx = ctx;
	escm_abort(e);
}
#endif

#define CHECK_PORT(e, a, inputp)                                            \
    do {                                                                    \
           if (!escm_type_ison(ESCM_TYPE_PORT)) {                           \
               escm_error(e, _(T("~s: port type is off.~%")), escm_fun(e)); \
            escm_abort(e);                                                  \
        }                                                                   \
        escm_assert(ESCM_ISPORT(a), a, e);                                  \
                                                                            \
        if (inputp != escm_port_val(a)->input) {                            \
            escm_error(e, _(T("~s: given port is not an correct port.~%")), \
                       escm_fun(e));                                        \
            escm_abort(e);                                                  \
        }                                                                   \
        if (escm_port_val(a)->closed) {                                     \
            escm_error(e, _(T("~s: port is closed.~%")), escm_fun(e));      \
            escm_abort(e);                                                  \
        }                                                                   \
    } while(0);

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_read_char(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;
    int c;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

#ifdef ESCM_USE_PORTS
    a = escm_cons_pop(e, &args);
    if (a) {
        CHECK_PORT(e, a, 1);
        c = escm_input_getc(escm_port_val(a)->d.input);
    } else
#endif
        c = escm_input_getc(e->input);

    if (c == EOF)
        return e->EOF_OBJ;

    return escm_char_make(e, c);
}

escm_atom *
escm_peek_char(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;
    int c;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

#ifdef ESCM_USE_PORTS
    a = escm_cons_pop(e, &args);
    if (a) {
        CHECK_PORT(e, a, 1);
        c = escm_input_peek(escm_port_val(a)->d.input);
    } else
#endif
        c = escm_input_peek(e->input);

    if (c == EOF)
        return e->EOF_OBJ;

    return escm_char_make(e, c);
}

escm_atom *
escm_write_char(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c, *a;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

#ifdef ESCM_USE_PORTS
    a = escm_cons_pop(e, &args);
    if (a) {
        CHECK_PORT(e, a, 0);
        escm_putc(escm_port_val(a)->d.output, escm_char_val(c));
    } else
#endif
        escm_putc(e->output, escm_char_val(c));

    return NULL;
}

escm_atom *
escm_unread_char(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c, *a;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

#ifdef ESCM_USE_PORTS
    a = escm_cons_pop(e, &args);
    if (a) {
        CHECK_PORT(e, a, 1);
        escm_input_ungetc(escm_port_val(a)->d.input, escm_char_val(c));
    } else
#endif
        escm_input_ungetc(e->input, escm_char_val(c));

    return NULL;
}
#endif /* ESCM_USE_CHARACTERS */

escm_atom *
escm_read(escm *e, escm_atom *args, void *nil)
{
#ifdef ESCM_USE_PORTS
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    if (a) {
        CHECK_PORT(e, a, 1);
        return escm_parse(e, escm_port_val(a)->d.input);
    } else
#endif
        return escm_parse(e, e->input);
}

escm_atom *
escm_write(escm *e, escm_atom *args, void *nil)
{
    escm_atom *atom, *p;

    (void) nil;
    atom = escm_cons_pop(e, &args);

#ifdef ESCM_USE_PORTS
    p = escm_cons_pop(e, &args);
    if (p) {
        CHECK_PORT(e, p, 0);
        escm_atom_print4(e, atom, escm_port_val(p)->d.output, 0);
    } else
#endif
        escm_atom_write(e,  atom);

    return NULL;
}

escm_atom *
escm_display(escm *e, escm_atom *args, void *nil)
{
    escm_atom *atom, *p;

    (void) nil;
    atom = escm_cons_pop(e, &args);

#ifdef ESCM_USE_PORTS
    p = escm_cons_pop(e, &args);
    if (p) {
         CHECK_PORT(e, p, 0);
        escm_atom_print4(e, atom, escm_port_val(p)->d.output, 1);
    } else
#endif
        escm_atom_display(e,  atom);

    return NULL;
}

escm_atom *
escm_newline(escm *e, escm_atom *args, void *nil)
{
#ifdef ESCM_USE_PORTS
    escm_atom *p;

    (void) nil;
    p = escm_cons_pop(e, &args);
    if (p) {
        CHECK_PORT(e, p, 0);
        escm_putc(escm_port_val(p)->d.output, T('\n'));
    } else
#endif
        escm_printf(e->output, T("\n"));

    return NULL;
}

escm_atom *
escm_gc(escm *e, escm_atom *args, void *nil)
{
    (void) args; (void) nil;

    escm_gc_collect(e);

    return NULL;
}

escm_atom *
escm_backtrace(escm *e, escm_atom *args, void *nil)
{
    (void) args; (void) nil;

    escm_print_backtrace(e, e->output);

    return NULL;
}

escm_atom *
escm_read_only_x(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) nil;

    escm_cons_car(args)->ro = 1;

    return NULL;
}

escm_atom *
escm_read_only_p(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) nil;

    return escm_cons_car(args)->ro ? e->TRUE : e->FALSE;
}

escm_atom *
escm_prim_assert(escm *e, escm_atom *args, void *nil)
{
    escm_atom *test, *ret;

    (void) nil;
    test = escm_cons_pop(e, &args);
    ret = escm_atom_eval(e, test);
    if (e->err == 1)
        return NULL;
    if (!ESCM_ISTRUE(e, ret)) {
        escm_error(e, _(T("assert failed: ~s.~%")), test);
        escm_abort(e);
    }

    return NULL;
}

escm_atom *
escm_set_case_sensitive_x(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    e->casesensitive = ESCM_ISTRUE(e, escm_cons_car(args));
    return NULL;
}

escm_atom *
escm_set_brackets_parens_x(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    e->brackets = ESCM_ISTRUE(e, escm_cons_car(args));
    return NULL;
}

escm_atom *
escm_set_print_backtrace_x(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    e->backtrace = ESCM_ISTRUE(e, escm_cons_car(args));
    return NULL;
}

escm_atom *
escm_exit(escm *e, escm_atom *args, void *nil)
{
    (void) args; (void) nil;
    e->quit = 1;
    return NULL;
}

escm_atom *
escm_test(escm *e, escm_atom *args, void *nil)
{
    escm_atom *name, *get;

    (void) nil;
    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(name), name, e);
    get = escm_cons_pop(e, &args);

    if (escm_atom_equal(e, get, escm_cons_car(args), 2))
        escm_notice(e, _(T("~s: passed.~%")), name);
    else {
        if (get)
            escm_error(e, _(T("~s: ~s expected, got ~s.~%")), name,
                       escm_cons_car(args), get);
        else
            escm_error(e, _(T("~s: ~s expected, got nothing.~%")), name,
                       escm_cons_car(args));
    }

    return NULL;
}

escm_atom *
escm_test_error(escm *e, escm_atom *args, void *nil)
{
    escm_atom *name, *test, *ret;
    escm_output *save;

    (void) nil;
    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(name), name, e);
    test = escm_cons_pop(e, &args);

    save = e->errp, e->errp = NULL; /* prevent error messages to be
                                       displayed */
    ret = escm_atom_eval(e, test);
    e->errp = save;
    if (e->err == 1) {
        escm_notice(e, _(T("~s: passed.~%")), name);
        e->err = 0;
    } else {
        if (ret)
            escm_error(e, _(T("~s: error expected, got ~s.~%")), name, ret);
        else
            escm_error(e, _(T("~s: error expected, got nothing.~%")), name);
    }

    return NULL;
}

#ifdef ESCM_USE_STRINGS
escm_atom *
escm_prim_printf(escm *e, escm_atom *args, void *nil)
{
    escm_atom *format;
	struct e_args s;

    (void) nil;
    format = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(format), format, e);

	s.e = e;
	s.args = args;
    escm_scmpf_fun(e, e->output, escm_str_val(format), &s,
				   (Escm_Fun_Next_Args) next_arg);

    return NULL;
}

escm_atom *
escm_format(escm *e, escm_atom *args, void *nil)
{
    escm_atom *atom;
    escm_output *out;
	struct e_args s;

    (void) nil;
    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(atom), atom, e);

    out = escm_output_str();

	s.e = e;
	s.args = args;
    escm_scmpf_fun(e, out, escm_str_val(atom), &s,
				   (Escm_Fun_Next_Args) next_arg);

    atom = escm_string_make(e, escm_output_getstr(out),
                            out->d.str.cur - out->d.str.str);
    escm_output_close(out);

    return atom;
}
#endif

static escm_atom *
named_let(escm *e, escm_atom *name, escm_atom *args)
{
    escm_atom *bindings, *val;
    escm_atom *fun, *env;
    escm_cons *c, *cons;

    bindings = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(bindings), bindings, e);
    if (args == e->NIL) {
        escm_error(e, _(T("~s: missing body.~%")), escm_fun(e));
        escm_abort(e);
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
    while (args != e->NIL)
        escm_ctx_put(e, escm_cons_pop(e, &args)); /* the body */

    fun = escm_lambda(e, escm_ctx_leave(e), NULL);
    if (!fun)
        escm_abort(e);
    escm_gc_gard(e, fun);

    /* build the new environment */
    env = escm_env_new(e, e->env);
    escm_proc_val(fun)->d.closure.env = env;
    escm_proc_val(fun)->name = tcsdup(escm_sym_name(name));

    escm_env_set(e, env, name, fun);

    e->ctx->tailrec = 1;
    escm_ctx_enter(e); /* now we create a list of the arguments */

    for (c = escm_cons_val(bindings); c; c = escm_cons_next(c))
        escm_ctx_put(e, escm_cons_car(escm_cons_cdr(c->car)));

    val = escm_procedure_exec(e, fun, escm_ctx_first(e), 1);
    escm_ctx_discard(e);
    escm_gc_ungard(e, fun);

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
        if (ESCM_ISSYM(c->car)) {
            /* this form can be found when adding a unquote after a dot
               (ie `(1 2 . ,a) -> `(1 2 unquote a) */
            if (0 == tcscmp(escm_sym_name(c->car), T("unquote"))) {
                if (c->cdr == e->NIL) {
                    escm_error(e, _(T("~s: unquote expect exactly one "))
                               _(T("argument.~%")), escm_fun(e));
                    goto err;
                }
                ret = quasiquote(e, escm_cons_val(c->cdr)->car, lvl - 1);
                if (!ret)
                    goto err;
                e->ctx->dotted = 1;
                escm_ctx_put(e, ret);
                break;
            } else if (0 == tcscmp(escm_sym_name(c->car),
                                    T("unquote-splicing"))) {
                escm_error(e, _(T("~s: unquote-splicing found after a "))
                           _(T("dotted notation.~%")), escm_fun(e));
                goto err;
            } else
                escm_ctx_put(e, c->car);
        } else if (ESCM_ISCONS(c->car)) {
            if (ESCM_ISSYM(escm_cons_car(c->car))) {
                escm_cons *cons;

                cons = escm_cons_val(c->car);
                if (0 == tcscmp(escm_sym_name(cons->car), T("unquote"))) {
                    if (cons->cdr == e->NIL) {
                        escm_error(e, _(T("~s: unquote expect exactly one "))
                                   _(T("argument.~%")), escm_fun(e));
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
                } else if (0 == tcscmp(escm_sym_name(cons->car),
                                        T("unquote-splicing"))) {
                    if (cons->cdr == e->NIL) {
                        escm_error(e, _(T("~s: unquote-splicing expect "))
                                   _(T("exactly one argument.~%")),
                                   escm_fun(e));
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
                        escm_error(e, _(T("~s: unquote-splicing expect a "))
                                   _(T("argument of type <list>.~%")),
                                   escm_fun(e));
                        goto err;
                    }
                    if (lvl != 1) {
                        escm_ctx_put(e, ret);
                        escm_ctx_put(e, escm_ctx_leave(e));
                    } else
                        escm_ctx_put_splicing(e, ret);
                } else if (0 == tcscmp(escm_sym_name(cons->car),
                                        T("quasiquote"))
                    && cons->cdr != e->NIL) {
                    escm_ctx_enter(e);
                    escm_ctx_put(e, cons->car);
                    ret = quasiquote(e, escm_cons_val(cons->cdr)->car, lvl + 1);
                    if (!ret)
                        goto err;
                    escm_ctx_put(e, ret);
                    escm_ctx_put(e, escm_ctx_leave(e));
                } else
                    goto nospecialcase;
            } else {
            nospecialcase:
                ret = quasiquote(e, c->car, lvl);
                if (!ret)
                    goto err;
                escm_ctx_put(e, ret);
            }
        } else
            escm_ctx_put(e, c->car);

        if (!ESCM_ISCONS(c->cdr)) {
            e->ctx->dotted = 1;
            escm_ctx_put(e, c->cdr);
        }
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

    if (!escm_type_ison(ESCM_TYPE_VECTOR)) {
        escm_error(e, _(T("~s: vector type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    escm_ctx_enter(e);

    v = escm_vector_val(atom);
    for (i = 0; i < v->len; i++) {
        if (ESCM_ISCONS(v->vec[i])) {
            if (ESCM_ISSYM(escm_cons_val(v->vec[i])->car)) {
                escm_cons *cons;

                cons = escm_cons_val(v->vec[i]);
                if (0 == tcscmp(escm_sym_name(cons->car), T("unquote"))) {
                    if (cons->cdr == e->NIL) {
                        escm_error(e, _(T("~s: unquote expect exactly one "))
                                   _(T("argument.~%")), escm_fun(e));
                        goto err;
                    }
                    if (lvl != 1) {
                        escm_ctx_enter(e);
                        escm_ctx_put(e, cons->car);
                    }
                    ret = quasiquote(e, escm_cons_val(cons->cdr)->car,
                                     lvl - 1);
                    if (!ret)
                        goto err;
                    escm_ctx_put(e, ret);
                    if (lvl != 1)
                        escm_ctx_put(e, escm_ctx_leave(e));
                } else if (0 == tcscmp(escm_sym_name(cons->car),
                                        T("unquote-splicing"))) {
                    if (cons->cdr == e->NIL) {
                        escm_error(e, _(T("~s: unquote expect exactly one "))
                                   _(T("argument.~%")), escm_fun(e));
                        goto err;
                    }
                    if (lvl != 1) {
                        escm_ctx_enter(e);
                        escm_ctx_put(e, cons->car);
                    }
                    ret = quasiquote(e, escm_cons_val(cons->cdr)->car,
                                     lvl - 1);
                    if (!ret)
                        goto err;
                    else if (!ESCM_ISCONS(ret)) {
                        escm_error(e, _(T("~s: unquote-splicing expect a "))
                                   _(T("argument of type <list>.~%")),
                                   escm_fun(e));
                        goto err;
                    }
                    if (lvl != 1) {
                        escm_ctx_put(e, ret);
                        escm_ctx_put(e, escm_ctx_leave(e));
                    } else
                        escm_ctx_put_splicing(e, ret);
                } else if (0 == tcscmp(escm_sym_name(cons->car),
                                        T("quasiquote"))
                    && cons->cdr != e->NIL) {
                    escm_ctx_enter(e);
                    escm_ctx_put(e, cons->car);
                    ret = quasiquote(e, escm_cons_val(cons->cdr)->car,
                                     lvl + 1);
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

    return escm_prim_vector(e, escm_ctx_leave(e), NULL);

err:
    escm_ctx_discard(e);
    return NULL;
}
#endif

static escm_atom *
next_arg(struct e_args *s)
{
	return escm_cons_pop(s->e, &(s->args));
}

