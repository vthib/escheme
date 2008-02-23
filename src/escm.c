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
#include <stdarg.h>
#include <ctype.h>

#include "escheme.h"

#ifndef ESCM_NSEG
# define ESCM_NSEG 10
#endif

#ifndef ESCM_CPSEG
# define ESCM_CPSEG 2000
#endif

struct escm_slist {
    escm_atom *atom;
    struct escm_slist *prev;
};

static unsigned char *alloc_seg(escm *);
static escm_atom *enterin(escm *, const char *);

escm *
escm_new(void)
{
    escm *e;
    size_t i;

    e = xcalloc(1, sizeof *e);
    e->segments = xcalloc(ESCM_NSEG, sizeof *e->segments);
    for (i = 0; i < ESCM_NSEG; i++)
	e->segments[i] = alloc_seg(e);

    e->input = escm_input_fmng(stdin, "standard input");
    e->output = escm_output_fmng(stdout, "standard output");
    e->errp = escm_output_fmng(stderr, "standard error output");

    e->brackets = 1;

    /* ! Do not change the order ! */
    escm_environments_init(e);
    e->env = escm_env_new(e, NULL);
    e->env->ro = 1;
    escm_env_addprimitives(e);

    escm_procedures_init(e);
    escm_symbols_init(e);
    escm_cons_init(e);

    return e;
}

void
escm_free(escm *e)
{
    escm_atom *atom;
    struct escm_slist *node, *prev;
    size_t i;

    assert(e != NULL);

    for (atom = e->heap; atom; atom = atom->link)
	escm_atom_free(e, atom);

    for (i = 0; i < e->ntypes; i++) {
	if (e->types[i]->type == TYPE_BUILT && e->types[i]->d.c.fexit)
	    e->types[i]->d.c.fexit(e, e->types[i]->d.c.dexit);
	free(e->types[i]);
    }
    free(e->types);

    for (node = e->gard; node; node = prev)
	prev = node->prev, free(node);

    for (i = 0; i < ESCM_NSEG; i++)
	free(e->segments[i]);
    free(e->segments);

    escm_input_close(e->input);
    escm_output_close(e->output);
    escm_output_close(e->errp);

    free(e);
}

int
escm_fparse(escm *e, const char *filename)
{
    escm_atom *atom;
    escm_input *save;

    assert(e != NULL);

    save = e->input, e->input = escm_input_fopen(filename);
    if (!e->input) {
	e->input = save;
	return 0;
    }
    while ((atom = escm_parse(e)) != e->EOF_OBJ) {
	if (atom) {
	    escm_atom_print(e, atom);
	    printf("\n");
	}
    }

    escm_input_close(e->input), e->input = save;
    return 1;
}

int
escm_sparse(escm *e, const char *str)
{
    escm_atom *atom;
    escm_input *save;

    assert(e != NULL);

    save = e->input;
#ifdef ESCM_USE_UNICODE
    {
	wchar_t *w;

	w = strtowcs(str);
	e->input = escm_input_str(w);
	free(w);
    }
#else
    e->input = escm_input_str(str);
#endif

    if (!e->input) {
	e->input = save;
	return 0;
    }
    while ((atom = escm_parse(e)) != e->EOF_OBJ) {
	if (atom) {
	    escm_atom_print(e, atom);
	    printf("\n");
	}
    }

    escm_input_close(e->input), e->input = save;
    return 1;
}

void
escm_shell(escm *e)
{
    escm_atom *atom;
    escm_input *old;
    int stdinp;

    assert(e != NULL);

    stdinp = (e->input->type == INPUT_FILE && e->input->d.file.fp == stdin);
    if (!stdinp)
	old = e->input, e->input = escm_input_fmng(stdin, "standard input");

    atom = NULL;
    do {
	if (atom) {
	    escm_atom_print(e, atom);
	    printf("\n");
	}
	printf("> ");
	if (EOF == fflush(stdout))
	    perror("fflush");
	atom = escm_parse(e);
    } while (e->input->end == 0);

    if (!stdinp)
	escm_input_close(e->input), e->input = old;
}
    
escm_atom *
escm_parse(escm *e)
{
    escm_atom *atom, *ret;
    size_t i;
    int c;

    assert(e != NULL);
    if (!e->input)
	return NULL;
    if (e->input->end)
	return e->EOF_OBJ;

    e->err = 0;

    do {
	c = escm_input_getc(e->input);
	if (c == '.') {
	    int c2;

	    if (!e->ctx)
		break;
	    c2 = escm_input_getc(e->input);
	    if (c2 == '.') {
		escm_input_ungetc(e->input, c2);
		break;
	    } else if (!isdigit(c2))
		e->ctx->dotted = 1;
	    c = c2;
	} else if (c == ';') {
	    while (c != '\n')
		c = escm_input_getc(e->input);
	}
    } while (isspace(c) && e->input->end == 0);
    if (e->input->end)
	return e->EOF_OBJ;

    if (c == '\'')
	atom = enterin(e, "quote");
    else if (c == '`')
	atom = enterin(e, "quasiquote");
    else if (c == ',') {
	int c2;

	c2 = escm_input_getc(e->input);
	if (c2 == '@')
	    atom = enterin(e, "unquote-splicing");
	else {
	    escm_input_ungetc(e->input, c2);
	    atom = enterin(e, "unquote");
	}
    } else {
	atom = NULL;
	for (i = 0; i < e->ntypes && !atom; i++) {
	    if (e->types[i]->type == TYPE_BUILT) {
		if (e->types[i]->d.c.fparsetest) {
		    if (e->types[i]->d.c.fparsetest(e, c)) {
			escm_input_ungetc(e->input, c);
			if (!e->types[i]->d.c.fparse)
			    continue;
			atom = e->types[i]->d.c.fparse(e);
			break;
		    }
		}
#ifdef ESCM_USE_CHARACTERS
	    } else {
		if (e->types[i]->d.dyn.fparsetest) {
		    atom = escm_procedure_exec(e, e->types[i]->d.dyn.fparsetest,
					       escm_char_make(e, c), 0);
		    if (ESCM_ISTRUE(atom)) {
			escm_input_ungetc(e->input, c);
			if (!e->types[i]->d.dyn.fparse)
			    continue;
			atom = escm_procedure_exec(e, e->types[i]->d.dyn.fparse,
						   e->NIL, 0);
			break;
		    }
		}
#endif
	    }
	}
	if (i >= e->ntypes) {
	    fprintf(stderr, "unknown character `%c'.\n", c);
	    escm_abort(e);
	}
    }

/*    c = escm_input_getc(e->input);
    if (c != '\n')
    escm_input_ungetc(e->input, c);*/

    if (!atom || e->ctx != NULL)
	return atom;

    escm_gc_gard(e, atom);
    ret = escm_atom_eval(e, atom);
    escm_gc_ungard(e, atom);
    return ret;
}

unsigned long
escm_type_add(escm *e, escm_type *type)
{
    assert(e != NULL);
    assert(type != NULL);

    e->ntypes++;
    if (e->ntypes % 2 == 1)
	e->types = xrealloc(e->types, (e->ntypes + 1) * sizeof *type);
    e->types[e->ntypes - 1] = type;

    return e->ntypes - 1;
}

void
escm_ctx_enter(escm *e)
{
    escm_context *ctx;

    assert(e != NULL);

    ctx = xcalloc(1, sizeof *ctx);
    ctx->prev = e->ctx;
    e->ctx = ctx;
}

void
escm_ctx_put(escm *e, escm_atom *atom)
{
    escm_atom *new;

    assert(e != NULL);

    if (!e->ctx || !atom)
	return;

    if (e->ctx->dotted) {
	new = atom;
	e->ctx->dotted = 0;
    } else
	new = escm_cons_make(e, atom, e->NIL);

    if (!e->ctx->first)
	e->ctx->first = new;
    else {
	if (!ESCM_ISCONS(e->ctx->last) || e->ctx->last == e->NIL) {
	    escm_input_print(e->input, "illegal dotted form.");
	    e->err = 1;
	    return;
	}
	escm_cons_val(e->ctx->last)->cdr = new;
    }

    e->ctx->last = new;
}

void
escm_ctx_put_splicing(escm *e, escm_atom *atom)
{
    escm_cons *c;

    assert(e != NULL);
    assert(atom != NULL);

    if (!e->ctx || atom == e->NIL)
	return;

    if (!ESCM_ISCONS(atom))
	escm_ctx_put(e, atom);

    e->ctx->dotted = 0;

    if (!e->ctx->first)
	e->ctx->first = atom;
    else {
	if (!ESCM_ISCONS(e->ctx->last)) { /* it's a "foo . bar" */
	    escm_input_print(e->input, "')' expected.");
	    e->err = 1;
	    return;
	}
	escm_cons_val(e->ctx->last)->cdr = atom;
    }

    if (ESCM_ISCONS(atom)) {
	escm_atom *obj;

	obj = atom;
	for (c = escm_cons_val(atom); c->cdr != e->NIL && ESCM_ISCONS(c->cdr);
	     c = escm_cons_next(c))
	    obj = c->cdr;
	e->ctx->last = (c->cdr == e->NIL) ? obj : c->cdr;
    } else
	e->ctx->last = atom;
}

escm_atom *
escm_ctx_first(escm *e)
{
    assert(e != NULL);
    if (!e->ctx)
	return NULL;

    return (e->ctx->first) ? e->ctx->first : e->NIL;
}

escm_atom *
escm_ctx_leave(escm *e)
{
    escm_atom *ret;

    assert(e != NULL);

    ret = escm_ctx_first(e);
    escm_ctx_discard(e);

    return ret;
}

void
escm_ctx_discard(escm *e)
{
    escm_context *old;

    assert(e != NULL);
    if (!e->ctx)
	return;

    old = e->ctx;
    e->ctx = old->prev;
    free(old);
}

void
escm_gc_collect(escm *e)
{
    escm_atom *white, *black, *c, *next;
    escm_context *ctx;
    struct escm_slist *li;
    size_t i;

    /* we mark all the known atoms */
    for (ctx = e->ctx; ctx; ctx = ctx->prev)
	escm_atom_mark(e, ctx->first);

    /* plus the garded ones */
    for (li = e->gard; li; li = li->prev)
	escm_atom_mark(e, li->atom);

    /* plus the possible dynamic-types procedures */
    for (i = 0; i < e->ntypes; i++) {
	if (e->types[i]->type == TYPE_DYN) {
	    escm_atom_mark(e, e->types[i]->d.dyn.fequal);
	    escm_atom_mark(e, e->types[i]->d.dyn.feval);
	    escm_atom_mark(e, e->types[i]->d.dyn.fparsetest);
	    escm_atom_mark(e, e->types[i]->d.dyn.fparse);
	}
    }

    escm_atom_mark(e, e->env);
    escm_atom_mark(e, e->NIL);

    escm_atom_mark(e, e->TRUE);
    escm_atom_mark(e, e->FALSE);

    /* then we separate blacks and whites */
    white = NULL, black = NULL;
    for (c = e->heap; c; c = next) {
	next = c->link;
	if (c->marked)
	    c->link = white, white = c;
	else
	    c->link = black, black = c;
    }

    /* now we clean the whites */
    for (c = white; c; c = c->link)
	c->marked = 0;
    e->heap = white;

    /* and free the blacks */
    for (c = black; c; c = next) {
	next = c->link;
	escm_atom_free(e, c);
	c->link = e->freelist, e->freelist = c;
    }
}

void
escm_gc_gard(escm *e, escm_atom *a)
{
    struct escm_slist *li;

    assert(e != NULL);
    assert(a != NULL);

    li = xmalloc(sizeof *li);
    li->prev = e->gard, e->gard = li;
    li->atom = a;
}

void
escm_gc_ungard(escm *e, escm_atom *a)
{
    struct escm_slist *li, *prev;

    assert(e != NULL);
    assert(a != NULL);

    prev = NULL;
    for (li = e->gard; li && li->atom != a; li = li->prev)
	prev = li;
    if (!prev && li)
	e->gard = li->prev;
    if (li) {
	if (prev)
	    prev->prev = li->prev;
	free(li);
    }
}

void
escm_tailrec(escm *e, escm_atom *cons, int eval)
{
    if (!e->tailrec || !ESCM_ISCONS(cons))
	return;

    escm_tailrec3(e, escm_cons_car(cons), escm_cons_cdr(cons), eval);
}

void
escm_tailrec3(escm *e, escm_atom *fun, escm_atom *args, int eval)
{
    escm_context *ctx, *c, *prev;
    escm_atom *a;

    if (!e->tailrec || !ESCM_ISCONS(args))
	return;

    fun = escm_atom_eval(e, fun);
    if (!fun || e->err == 1)
	return;

    for (ctx = e->ctx; ctx && ctx->fun != fun; ctx = ctx->prev)
	;
    if (!ctx)
	return;

    /* we have a match, now we eval the new args */
    escm_ctx_enter(e);
    for (a = escm_cons_pop(e, &args); a; a = escm_cons_pop(e, &args)) {
	if (eval) {
	    a = escm_atom_eval(e, a);
	    if (!a || e->err == 1) {
		escm_ctx_discard(e);
		return;
	    }
	}
	escm_ctx_put(e, a);
    }
    ctx->first = escm_ctx_leave(e);

    /* clean the contexts */
    for (c = e->ctx; c != ctx; c = prev) {
	prev = c->prev;
	free(c);
    }

    /* and jump */
    e->tailrec = 0;
    e->ctx = ctx;
    longjmp(ctx->jbuf, 1);
}

/* privates functions */

static unsigned char *
alloc_seg(escm *e)
{
    escm_atom *seg;
    escm_atom *c;
    long i;

    seg = xcalloc(ESCM_CPSEG, sizeof *seg);
    for (i = ESCM_CPSEG - 1; i >= 0; i--) {
	c = &seg[i];
	c->link = e->freelist;
	e->freelist = c;
    }

    return (unsigned char *) seg;
}

static escm_atom *
enterin(escm *e, const char *name)
{
    escm_atom *atom;

    assert(e != NULL);

    escm_ctx_enter(e);
    escm_ctx_put(e, escm_symbol_make(e, name));
    atom = escm_parse(e);
    if (!atom) {
	escm_ctx_discard(e);
	return NULL;
    }
    escm_ctx_put(e, atom);
    return escm_ctx_leave(e);
}
