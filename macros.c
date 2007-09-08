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

#include "escheme.h"

typedef struct escm_match escm_match;

struct escm_match {
    escm_atom *ident;
    struct {
	escm_atom *fst;
	escm_atom *lst;
    } val;

    escm_match *prev;
};

static size_t macrotype = 0;

static int checksym(escm *, escm_atom *);

static int match(escm *, escm_macro *, escm_atom *, escm_atom *);
static escm_match *bind(escm *, escm_macro *, escm_atom *, escm_atom *,
			escm_match *);
static escm_atom *expand(escm *, escm_macro *, escm_atom *, escm_match *, int);
static escm_match *add(escm *, escm_match *, escm_atom *, escm_atom *);
static escm_match *checkup(escm *, escm_match *, escm_atom *);

static void macro_mark(escm *, escm_macro *);
static void macro_print(escm *, escm_macro *, FILE *);

void
escm_macros_init(escm *e)
{
    escm_type *t;
    escm_atom *o;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) macro_mark;
    t->ffree = (Escm_Fun_Free) free;
    t->fprint = (Escm_Fun_Print) macro_print;

    macrotype = escm_type_add(e, t);

    o = escm_procedure_new(e, "expand", 1, 1, escm_expand, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;
    o = escm_procedure_new(e, "syntax-rules", 1, -1, escm_syntax_rules, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
    o = escm_procedure_new(e, "define-syntax", 2, 2, escm_define_syntax, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
}

size_t
escm_macro_tget(void)
{
    return macrotype;
}

escm_atom *
escm_macro_expand(escm *e, escm_atom *macro, escm_atom *cont)
{
    escm_macro *m;
    escm_atom *rules, *a;
    escm_match *b, *prev;

    m = macro->ptr;
    rules = m->rules;
    for (a = escm_cons_pop(e, &rules); a; a = escm_cons_pop(e, &rules)) {
	if (match(e, m, escm_cons_car(a), cont)) {
#if 1
	    b = bind(e, m, escm_cons_car(a), cont, NULL);
	    a = expand(e, m, escm_cons_car(escm_cons_val(a)->cdr), b, 0);
	    while (b) {
		prev = b->prev;
		free(b);
		b = prev;
	    }
	    return a;
#else
	    escm_atom_display(e, escm_cons_car(a), stdout);
	    printf(" match ");
	    escm_atom_display(e, cont, stdout);
	    printf(".\n");
	    return NULL;
#endif
	}
    }

    fprintf(stderr, "can't expand macro ");
    escm_atom_display(e, cont, stderr);
    fprintf(stderr, ".\n");
    e->err = -1;
    return NULL;
}

escm_atom *
escm_expand(escm *e, escm_atom *args)
{
    escm_atom *arg;
    escm_atom *atom, *macro;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    atom = escm_cons_car(arg);
    macro = escm_atom_eval(e, atom);
    if (e->err == -1)
	return NULL;
    if (!macro) {
	escm_atom_display(e, atom, stderr);
	fprintf(stderr, ": expression do not yield an applicable value.\n");
	e->err = -1;
	return NULL;
    }
    if (!ESCM_ISMACRO(macro)) {
	escm_atom_display(e, macro, stderr);
	fprintf(stderr, ": not a macro.\n");
	e->err = -1;
	return NULL;
    }

    return escm_macro_expand(e, macro, arg);
}

escm_atom *
escm_define_syntax(escm *e, escm_atom *args)
{
    escm_atom *name, *val;

    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(name), name, e);

    val = escm_atom_eval(e, escm_cons_car(args));
    if (e->err == -1)
	return NULL;
    if (!val) {
	escm_atom_display(e, escm_cons_car(args), stderr);
	fprintf(stderr, ": expression not allowed in a definition.\n");
	e->err = -1;
	return NULL;
    }
    escm_assert(ESCM_ISMACRO(val), val, e);

    escm_env_set(e->env, escm_sym_val(name), val);

    return NULL;
}

escm_atom *
escm_syntax_rules(escm *e, escm_atom *args)
{
    escm_atom *a;
    escm_cons *c;
    escm_macro *m;

    m = xmalloc(sizeof *m);
    m->literals = escm_cons_pop(e, &args);
    escm_assert1(ESCM_ISCONS(m->literals), m->literals, e, free(m));

    m->rules = args;
    for (a = escm_cons_pop(e, &args); a; a = escm_cons_pop(e, &args)) {
	escm_assert1(ESCM_ISCONS(a), a, e, free(m));

	c = a->ptr;
	escm_assert1(ESCM_ISCONS(c->car), c->car, e, free(m));
	escm_assert1(c->cdr != e->NIL, a, e, free(m));

	if (!checksym(e, c->car)) {
	    fprintf(stderr, "invalid syntax-rules.\n");
	    e->err = -1;
	    return NULL;
	}
    }

    m->env = e->env;

    return escm_atom_new(e, macrotype, m);
}

static int
match(escm *e, escm_macro *m, escm_atom *m1, escm_atom *m2)
{
    escm_cons *rule, *cons;
    escm_atom *a1, *a2;

    /* assert(iscons(rule && cons)) */
    rule = m1->ptr, cons = m2->ptr;
    while (rule) {
	a1 = rule->car;
	a2 = (cons) ? cons->car : NULL;

	if (!a2) {
	    if (ESCM_ISSYM(a1) && 0 == strcmp(escm_sym_val(a1), "..."))
		return 1;
	    rule = escm_cons_next(rule);
	    if (rule && 0 == strcmp(escm_sym_val(rule->car), "..."))
		return 1;
	    return 0;
	}

	if (ESCM_ISCONS(a1)) {
	    if (!ESCM_ISCONS(a2))
		return 0;
	    if (!match(e, m, a1, a2))
		return 0;
	} else if (ESCM_ISSYM(a1)) {
	    if (escm_cons_isin(e, m->literals, a1)) {
		if (escm_env_getlocal(e->env, escm_sym_val(a1)))
		    return 0;
		if (!escm_atom_equal(e, a1, a2, 2))
		    return 0;
	    }
	    if (0 == strcmp(escm_sym_val(a1), "..."))
		return 1;
	} else {
	    if (!escm_atom_equal(e, a1, a2, 2))
		return 0;
	}

	if (!ESCM_ISCONS(rule->cdr)) /* rest arguments */
	    return 1;

	rule = escm_cons_next(rule);
	cons = escm_cons_next(cons);
    }

    return (cons == NULL);
}

static escm_match *
bind(escm *e, escm_macro *m, escm_atom *m1, escm_atom *m2, escm_match *match)
{
    escm_cons *rule, *cons;
    escm_atom *a1, *a2;

    rule = m1->ptr, cons = (m2) ? m2->ptr : NULL;
    while (rule) {
	a1 = rule->car;
	a2 = (cons) ? cons->car : NULL;

	if (!a1)
	    return match;

	if (!ESCM_ISCONS(rule->cdr)) /* rest arguments */
	    return add(e, match, rule->cdr, (cons) ? cons->cdr : e->NIL);

	rule = escm_cons_next(rule);
	if (rule != NULL && ESCM_ISSYM(rule->car) &&
	    0 == strcmp(escm_sym_val(rule->car), "...")) {
	    do {
		if (ESCM_ISCONS(a1))
		    match = bind(e, m, a1, a2, match);
		else
		    match = add(e, match, a1, a2);
		if (cons)
		    cons = escm_cons_next(cons), a2 = (cons) ? cons->car : NULL;
	    } while (a2);

	    return match;
	}

	if (ESCM_ISCONS(a1))
	    match = bind(e, m, a1, a2, match);
	else if (ESCM_ISSYM(a1) && !escm_cons_isin(e, m->literals, a1))
	    match = add(e, match, a1, a2);

	if (cons)
	    cons = escm_cons_next(cons);
    }

    return match;
}

static escm_atom *
expand(escm *e, escm_macro *m, escm_atom *tpl, escm_match *bind, int abort)
{
    escm_atom *a;
    escm_match *match;

    if (ESCM_ISSYM(tpl)) {
	match = checkup(e, bind, tpl);
	return (match) ? escm_cons_car(match->val.fst) : tpl;
    } else if (ESCM_ISCONS(tpl)) {
	escm_ctx_enter(e);

	for (a = escm_cons_pop(e, &tpl); a; a = escm_cons_pop(e, &tpl)) {
	    if (ESCM_ISSYM(a)) {
		if (0 == strcmp(escm_sym_val(a), "..."))
		    continue;

		match = checkup(e, bind, a);
		if (match != NULL) {
		    if (!match->val.fst) {
			if (!abort)
			    continue;
			escm_ctx_discard(e);
			return NULL;
		    }

		    if (tpl && ESCM_ISSYM(escm_cons_car(tpl)) &&
			0 == strcmp(escm_sym_val(escm_cons_car(tpl)), "..."))
			escm_ctx_put_splicing(e, match->val.fst);
		    else {
			escm_ctx_put(e, escm_cons_car(match->val.fst));
			if (abort) {
			    match->val.fst = escm_cons_val(match->val.fst)->cdr;
			    if (match->val.fst == e->NIL)
				match->val.fst = NULL;
			}
		    }
		} else {
		    if (!tpl || !ESCM_ISSYM(escm_cons_car(tpl)) ||
			0 != strcmp(escm_sym_val(escm_cons_car(tpl)), "..."))
		    escm_ctx_put(e, a);
		}
	    } else if (ESCM_ISCONS(a)) {
		if (tpl && ESCM_ISSYM(escm_cons_car(tpl)) &&
		    0 == strcmp(escm_sym_val(escm_cons_car(tpl)), "...")) {
		    escm_atom *ret;

		    ret = expand(e, m, a, bind, 1);
		    while (ret) {
			escm_ctx_put(e, ret);
			ret = expand(e, m, a, bind, 1);
		    }
		} else
		    escm_ctx_put(e, expand(e, m, a, bind, 0));
	    } else
		escm_ctx_put(e, a);
	}

	return escm_ctx_leave(e);
    } else
	return tpl;
}

static escm_match *
add(escm *e, escm_match *match, escm_atom *id, escm_atom *val)
{
    escm_match *m;
    escm_atom *cons;

    m = checkup(e, match, id);

    if (!m) {
	m = xcalloc(1, sizeof *m);
	m->ident = id;
	m->prev = match;
	match = m;
    }

    if (!val) {
	m->val.fst = NULL;
	return match;
    }

    cons = escm_cons_make(e, val, e->NIL);
    if (!m->val.fst)
	m->val.fst = cons, m->val.lst = cons;
    else
	escm_cons_val(m->val.lst)->cdr = cons, m->val.lst = cons;

    return match;
}

static escm_match *
checkup(escm *e, escm_match *match, escm_atom *id)
{
    escm_match *m;

    for (m = match; m && !escm_atom_equal(e, m->ident, id, 2); m = m->prev)
	;

    return m;
}

static int
checksym(escm *e, escm_atom *cons)
{
    escm_cons *c;

    for (c = cons->ptr; c; c = escm_cons_next(c)) {
	if (ESCM_ISCONS(c->car)) {
	    if (!checksym(e, c->car))
		return 0;
	}
	if (!ESCM_ISCONS(c->cdr))
	    return ESCM_ISSYM(c->cdr);
    }

    return 1;
}

static void
macro_mark(escm *e, escm_macro *m)
{
    escm_atom_mark(e, m->literals);
    escm_atom_mark(e, m->rules);
    escm_atom_mark(e, m->env);
}

static void
macro_print(escm *e, escm_macro *m, FILE *stream)
{
    (void) e;
    (void) m;

    fprintf(stream, "#<macro>");
}
