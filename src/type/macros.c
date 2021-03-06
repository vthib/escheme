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
#include <string.h>
#include <stdlib.h>

#include "macros.h"
#include "base.h"

/* Macro implementation :
 *
 * the macro implementation is pretty straightforward, except regarding
 * ellipsis.
 * Variable values are stored in the "match" linked list, and if the symbol
 * is "ellipsed", subsequent values are stored in the "node" linked list, each
 * group of values are ended with a NULL value, needed to separate the
 * differents groups (i.e. ((name sym ...) ...))
 */

/* TODO Rules should be allowed to be specified in vector form. This is not
 * implemented.
 */

struct node {
    escm_atom *a;
    struct node *next;
};

struct match {
    escm_atom *ident;
    struct node *fst;
    struct node *cur;

    struct match *prev;

    unsigned int ellipsed : 1;
};

static size_t macrotype = 0;

static int match(escm *, escm_macro *, escm_atom *, escm_atom *);
static struct match *bind(escm *, escm_macro *, escm_atom *, escm_atom *,
                          struct match *, int);
static escm_atom *expand(escm *, escm_macro *, escm_atom *, struct match *,
                         int);

static struct match *add(struct match *, escm_atom *, escm_atom *,
                         struct match *, int);
static struct match *checkup(escm *, struct match *, escm_atom *);

static void macro_mark(escm *, escm_macro *);
static void macro_print(escm *, escm_macro *, escm_output *, int);
static escm_atom *macro_exec(escm *, escm_macro *, escm_atom *);
static escm_atom *macro_expand(escm *, escm_macro *, escm_atom *);

void
escm_macros_init(escm *e)
{
    escm_type *t;
    escm_atom *o;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) macro_mark;
    t->ffree = (Escm_Fun_Free) free;
    t->print.fprint = (Escm_Fun_Print) macro_print;
    t->exec.fexec = (Escm_Fun_Exec) macro_exec;

    macrotype = escm_type_add(e, t);

    o = escm_procedure_new(e, T("expand"), 1, 1, escm_expand, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, T("syntax-rules"), 1, -1, escm_syntax_rules, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, T("define-syntax"), 2, 2, escm_define_syntax, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
}

size_t
escm_macro_tget(void)
{
    return macrotype;
}

escm_atom *
escm_expand(escm *e, escm_atom *args, void *nil)
{
    escm_atom *arg;
    escm_atom *atom, *macro;

    (void) nil;
    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    atom = escm_cons_car(arg);
    macro = escm_atom_eval(e, atom);
    if (e->err == 1)
        return NULL;
    if (!macro) {
        escm_error(e, _(T("~s: ~s do not yield an applicable value.~%")),
                   escm_fun(e), atom);
        escm_abort(e);
    }
    if (!ESCM_ISMACRO(macro)) {
        escm_error(e, _(T("~s: ~s is not a macro.~%")), escm_fun(e), macro);
        escm_abort(e);
    }

    return macro_expand(e, macro->ptr, escm_cons_cdr(arg));
}

escm_atom *
escm_define_syntax(escm *e, escm_atom *args, void *nil)
{
    escm_atom *name, *val;

    (void) nil;
    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(name), name, e);

    val = escm_atom_eval(e, escm_cons_car(args));
    if (e->err == 1)
        return NULL;
    if (!val) {
        escm_error(e, _(T("~s: ~s expression not allowed in a definition "))
                   "context.~%", escm_fun(e), escm_cons_car(args));
        escm_abort(e);
    }
    escm_assert(ESCM_ISMACRO(val), val, e);

    escm_symbol_set(name, val);

    return NULL;
}

escm_atom *
escm_syntax_rules(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;
    escm_cons *c;
    escm_macro *m;

    (void) nil;
    m = xmalloc(sizeof *m);
    m->literals = escm_cons_pop(e, &args);
    escm_assert1(ESCM_ISCONS(m->literals), m->literals, e, free(m));

    m->rules = args;
    for (a = escm_cons_pop(e, &args); a; a = escm_cons_pop(e, &args)) {
        escm_assert1(ESCM_ISCONS(a), a, e, free(m));

        c = a->ptr;
        escm_assert1(ESCM_ISCONS(c->car), c->car, e, free(m));
        escm_assert1(c->cdr != e->NIL, a, e, free(m));
    }

    m->env = e->env;

    return escm_atom_new(e, macrotype, m);
}

static int
match(escm *e, escm_macro *m, escm_atom *rule, escm_atom *arg)
{
    if (!rule || !arg)
        return !rule && !arg;

    if (ESCM_ISSYM(rule)) {
        if (escm_cons_isin(e, m->literals, rule, 1) &&
            escm_atom_equal(e, rule, arg, 1) == 0)
            return 0;
        return 1;
    } else if (ESCM_ISCONS(rule)) {
        escm_atom *r, *a;
        int nextcons;

        if (!ESCM_ISCONS(arg))
            return 0;
        if (rule == e->NIL)
            return arg == e->NIL;

        nextcons = !ESCM_ISCONS(escm_cons_cdr(rule));
        if (nextcons &&
            !match(e, m, escm_cons_cdr(rule), escm_cons_cdr(arg)))
            return 0;

        r = escm_cons_pop(e, &rule);
        a = escm_cons_pop(e, &arg);
        if (rule != e->NIL && ESCM_ISSYM(escm_cons_car(rule)) &&
            tcscmp(escm_sym_name(escm_cons_car(rule)), T("...")) == 0)
            return 1;
        if (!match(e, m, r, a))
            return 0;
        return (nextcons) ? 1 : match(e, m, rule, arg);
    } else
        return escm_atom_equal(e, rule, arg, 2);
}

static struct match *
bind(escm *e, escm_macro *m, escm_atom *rule, escm_atom *arg,
     struct match *match, int ellipsis)
{
    struct match *mid;

    if (rule == e->NIL)
        return match;

    if (ESCM_ISSYM(rule)) {
        if (!escm_cons_isin(e, m->literals, rule, 1))
            return add(match, rule, arg, checkup(e, match, rule), ellipsis);
    } else if (ESCM_ISCONS(rule)) {
        escm_atom *r, *a;

        if (!ESCM_ISCONS(escm_cons_cdr(rule)))
            match = bind(e, m, escm_cons_cdr(rule),
                         arg ? escm_cons_cdr(arg) : NULL, match, ellipsis);

        r = escm_cons_pop(e, &rule);
        if (rule != e->NIL && ESCM_ISSYM(escm_cons_car(rule)) &&
            tcscmp(escm_sym_name(escm_cons_car(rule)), T("...")) == 0) {
            if (ESCM_ISSYM(r)) {
                mid = checkup(e, match, r);
                while ((a = escm_cons_pop(e, &arg)) != NULL) {
                    match = add(match, r, a, mid, 1);
                    if (!mid)
                        mid = checkup(e, match, r);
                }
            } else {
                while ((a = escm_cons_pop(e, &arg)) != NULL)
                    match = bind(e, m, r, a, match, 1);
            }
            match = bind(e, m, r, a, match, 1);

            (void) escm_cons_pop(e, &rule);
        } else  {
            a = escm_cons_pop(e, &arg);
            match = bind(e, m, r, a, match, ellipsis);
        }

        return bind(e, m, rule, arg, match, ellipsis);
    }
    return match;
}

static escm_atom *
expand(escm *e, escm_macro *m, escm_atom *rule, struct match *match,
       int ellipsis)
{
    struct match *mid;
    escm_atom *atom;
    escm_atom *ret;

    if (ESCM_ISSYM(rule)) {
        mid = checkup(e, match, rule);
        if (mid) {
            if (ellipsis) {
                if (mid->ellipsed) {
                    if (!mid->cur)
                        return NULL;
                    if (!mid->cur->a) {
                        mid->cur = (mid->cur->next) ? mid->cur->next :
                            mid->fst;
                        return NULL;
                    }
                ret = mid->cur->a, mid->cur = mid->cur->next;
                } else
                    ret = mid->fst->a;
            } else
                ret = mid->fst->a;
        } else {
            atom = escm_sym_val(rule);
            if (!atom) /* this symbol will be set in the context (we hope) */
                ret = rule;
            else {
                ret = escm_atom_new(e, atom->type, atom->ptr);
                ret->noeval = 1, ret->nofree = 1;
            }
        }
    } else if (ESCM_ISCONS(rule)) {
        escm_cons *c;

        escm_ctx_enter(e);

        for (c = rule->ptr; c != NULL; c = escm_cons_next(c)) {
            if (ESCM_ISCONS(c->cdr) && c->cdr != e->NIL &&
                ESCM_ISSYM(escm_cons_car(c->cdr)) &&
                tcscmp(escm_sym_name(escm_cons_car(c->cdr)), T("...")) == 0) {
                while ((atom = expand(e, m, c->car, match, 1)) != NULL)
                    escm_ctx_put(e, atom);
                c = escm_cons_next(c);
            } else {
                atom = expand(e, m, c->car, match, ellipsis);
                if (!atom) goto retnull;
                escm_ctx_put(e, atom);
            }

            if (!ESCM_ISCONS(c->cdr)) {
                e->ctx->dotted = 1;
                atom = expand(e, m , c->cdr, match, ellipsis);
                if (!atom) goto retnull;
                escm_ctx_put(e, atom);
            }
        }

        ret = escm_ctx_leave(e);
    } else
        ret = rule;

    return ret;

retnull:
    escm_ctx_discard(e);
    return NULL;
}

static struct match *
add(struct match *match, escm_atom *id, escm_atom *bounded, struct match *m,
    int ellipsis)
{
    struct node *n;

    if (!m) {
        m = xcalloc(1, sizeof *m);
        m->ident = id, m->prev = match;
        match = m;
    }

    n = xmalloc(sizeof *n);
    n->a = bounded, n->next = NULL;
    if (!m->fst)
        m->fst = n, m->cur = n;
    else
        m->cur->next = n, m->cur = n;
    m->ellipsed = ellipsis;

    return match;
}

static struct match *
checkup(escm *e, struct match *match, escm_atom *id)
{
    struct match *m;

    for (m = match; m && !escm_atom_equal(e, m->ident, id, 2); m = m->prev)
        ;

    return m;
}

static void
macro_mark(escm *e, escm_macro *m)
{
    escm_atom_mark(e, m->literals);
    escm_atom_mark(e, m->rules);
    escm_atom_mark(e, m->env);
}

static void
macro_print(escm *e, escm_macro *m, escm_output *stream, int lvl)
{
    (void) e;
    (void) m;
    (void) lvl;

    escm_printf(stream, T("#<macro>"));
}

static escm_atom *
macro_exec(escm *e, escm_macro *m, escm_atom *args)
{
    escm_atom *a;

    a = macro_expand(e, m, args);
    return (a) ? escm_atom_eval(e, a) : a;
}

static escm_atom *
macro_expand(escm *e, escm_macro *m, escm_atom *args)
{
    escm_atom *rules, *a, *prevenv;
    struct match *b, *ma;
    struct node *n, *next;

    rules = m->rules;
    for (a = escm_cons_pop(e, &rules); a; a = escm_cons_pop(e, &rules)) {
        if (match(e, m, escm_cons_cdr(escm_cons_car(a)), args)) {
            b = bind(e, m, escm_cons_cdr(escm_cons_car(a)), args, NULL, 0);
            for (ma = b; ma; ma = ma->prev)
                ma->cur = ma->fst;

            prevenv = escm_env_enter(e, m->env);
            a = expand(e, m, escm_cons_car(escm_cons_cdr(a)), b, 0);
            for (; b; b = ma) {
                ma = b->prev;
                for (n = b->fst; n; n = next)
                    next = n->next, free(n);
                free(b);
            }
            escm_env_leave(e, prevenv);
            return a;
        }
    }

    escm_error(e, _(T("Can't expand macro ~s.~%")),
               escm_cons_car(escm_cons_car(escm_cons_car(m->rules))));
    escm_abort(e);
}
