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
#include <ctype.h>

#include "escm.h"
#include "escheme.h"

#ifndef ESCM_NSEG
# define ESCM_NSEG 20
#endif

#ifndef ESCM_CPSEG
# define ESCM_CPSEG 5000
#endif

struct escm_context {
    escm_atom *first;
    escm_atom *last;

    struct escm_context *prev;

    unsigned int dotted : 1;
};

struct escm_slist {
    escm_atom *atom;
    struct escm_slist *prev;
};

static unsigned char *alloc_seg(escm *);

escm *
escm_new(void)
{
    escm *e;
    size_t i;

    e = xcalloc(1, sizeof *e);
    e->segments = xcalloc(ESCM_NSEG, sizeof *e->segments);
    for (i = 0; i < ESCM_NSEG; i++)
	e->segments[i] = alloc_seg(e);

    /* XXX: do not change the order, or change the macros ESCM_TYPE_CONS etc! */
    escm_env_init(e);
    e->env = escm_env_new(e, NULL);

    escm_cons_init(e);
    escm_procedure_init(e);

#ifdef ESCM_USE_NUMBER
    escm_number_init(e);
#endif
#ifdef ESCM_USE_SYMBOL
    escm_symbol_init(e);
#endif
#ifdef ESCM_USE_STRING
    escm_string_init(e);
#endif
#ifdef ESCM_USE_BOOLEAN
    escm_boolean_init(e);
#endif

    escm_primitives_load(e);

    return e;
}

void
escm_free(escm *e)
{
    escm_atom *atom;
    size_t i;

    assert(e != NULL);

    for (atom = e->heap; atom; atom = atom->link)
	escm_atom_free(e, atom);

    for (i = 0; i < e->ntypes; i++)
	free(e->types[i]);
    free(e->types);

    for (i = 0; i < ESCM_NSEG; i++)
	free(e->segments[i]);
    free(e->segments);

    free(e);
}

escm_atom *
escm_fparse(escm *e, const char *filename)
{
    escm_atom *atom;

    assert(e != NULL);

    e->input = escm_input_fopen(filename, "r");
    atom = escm_parse(e);
    escm_input_close(e->input), e->input = NULL;

    return atom;
}

escm_atom *
escm_sparse(escm *e, const char *str)
{
    escm_atom *atom;

    assert(e != NULL);

    e->input = escm_input_str(str);
    atom = escm_parse(e);
    escm_input_close(e->input), e->input = NULL;

    return atom;
}

void
escm_shell(escm *e)
{
    escm_atom *atom;

    assert(e != NULL);

    e->input = escm_input_fmng(stdin, "standard input");
    do {
	printf("escheme> ");
	if (EOF == fflush(stdout))
	    perror("fflush");
	atom = escm_parse(e);
	if (atom) {
	    escm_atom_display(e, atom, stdout);
	    printf("\n");
	}
    } while (e->input->end == 0);

    escm_input_close(e->input), e->input = NULL;
}
    
escm_atom *
escm_parse(escm *e)
{
    escm_atom *atom, *ret;
    size_t i;
    int c;

    assert(e != NULL);
    if (!e->input || e->input->end)
	return NULL;

    e->err = 0;

    do {
	c = escm_input_getc(e->input);
	if (c == '.') {
	    e->dotted = 1;
	    c = escm_input_getc(e->input);
	} 
    } while (isspace(c) && e->input->end == 0);
    if (e->input->end)
	return NULL;

    if (c == '\'') {
	escm_ctx_enter(e);
	escm_ctx_put(e, e->QUOTE);
	atom = escm_parse(e);
	if (!atom) {
	    escm_ctx_discard(e);
	    return NULL;
	}
	escm_ctx_put(e, atom);
	atom = escm_ctx_leave(e);
    } else {
	atom = NULL;
	for (i = 0; i < e->ntypes && !atom; i++) {
	    if (e->types[i]->fparsetest) {
		if (e->types[i]->fparsetest(e, c)) {
		    escm_input_ungetc(e->input, c);
		    /* XXX: check fparse for non NULL */
		    atom = e->types[i]->fparse(e);
		    break;
		}
	    }
	}
	if (i >= e->ntypes) {
	    if (e->quiet)
		e->err = c;
	    else
		fprintf(stderr, "unknown character `%c'.\n", c);
	    return NULL;
	}
    }

    if (!atom || e->ctx != NULL)
	return atom;

    escm_gc_gard(e, atom);
    ret = escm_atom_eval(e, atom);
    escm_gc_ungard(e, atom);
    return ret;
}

size_t
escm_type_add(escm *e, escm_type *type)
{
    assert(e != NULL);
    assert(type != NULL);

    e->ntypes++;
    e->types = xrealloc(e->types, e->ntypes * sizeof *type);
    e->types[e->ntypes - 1] = type;

    return e->ntypes - 1;
}

void
escm_ctx_enter(escm *e)
{
    struct escm_context *ctx;

    assert(e != NULL);

    ctx = xcalloc(1, sizeof *ctx);
    ctx->prev = e->ctx;
    e->ctx = ctx;

    e->ctx->dotted = e->dotted;
    e->dotted = 0;
}

void
escm_ctx_put(escm *e, escm_atom *atom)
{
    escm_atom *new;

    assert(e != NULL);
    assert(atom != NULL);

    if (!e->ctx)
	return;

    if (e->dotted) {
	new = atom;
	e->dotted = 0;
    } else
	new = escm_cons_new(e, atom, e->NIL);

    if (!e->ctx->first)
	e->ctx->first = new;
    else {
	if (!ESCM_ISCONS(e->ctx->last)) { /* it's a "foo . bar" */
	    escm_input_print(e->input, "')' expected.\n");
	    e->err = -1;
	    return;
	}
	ESCM_CONS_VAL(e->ctx->last)->cdr = new;
    }

    e->ctx->last = new;
}

void
escm_ctx_put_splicing(escm *e, escm_atom *atom)
{
    escm_cons *c;

    assert(e != NULL);
    assert(atom != NULL);

    if (!e->ctx)
	return;

    e->dotted = 0;

    if (!e->ctx->first)
	e->ctx->first = atom;
    else {
	if (!ESCM_ISCONS(e->ctx->last)) { /* it's a "foo . bar" */
	    escm_input_print(e->input, "')' expected.\n");
	    e->err = -1;
	    return;
	}
	ESCM_CONS_VAL(e->ctx->last)->cdr = atom;
    }

    if (ESCM_ISCONS(atom)) {
	escm_atom *obj;

	obj = atom;
	for (c = ESCM_CONS_VAL(atom); c->cdr != e->NIL && ESCM_ISCONS(c->cdr);
	     c = ESCM_CONS_NEXT(c))
	    obj = c->cdr;
	e->ctx->last = (c->cdr == e->NIL) ? obj : c->cdr;
    } else
	e->ctx->last = atom;
}

escm_atom *
escm_ctx_first(escm *e)
{
    assert (e != NULL);
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
    struct escm_context *old;

    assert(e != NULL);
    if (!e->ctx)
	return;

    old = e->ctx;
    e->dotted = old->dotted;
    e->ctx = old->prev;
    free(old);
}

void
escm_gc_collect(escm *e)
{
    escm_atom *white, *black, *c, *next;
    struct escm_context *ctx;
    struct escm_slist *li;

    /* we mark all the known atoms */
    for (ctx = e->ctx; ctx; ctx = ctx->prev)
	escm_atom_mark(e, ctx->first);

    /* plus the garded ones */
    for (li = e->gard; li; li = li->prev)
	escm_atom_mark(e, li->atom);

    escm_atom_mark(e, e->env);

    escm_atom_mark(e, e->NIL);
    escm_atom_mark(e, e->QUOTE);

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

    for (li = e->gard; li->atom != a; li = prev) {
	prev = li->prev;
	free(li);
    }
    e->gard = li->prev;
    free(li);
}

/*--- privates functions ---*/

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
