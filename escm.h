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
#ifndef ESCHEME_ESCM_H
# define ESCHEME_ESCM_H

#include <stdio.h>
#include "types.h"
#include "cons.h"

#define escm_assert(test, atom, e)		\
    if (!(test)) {				\
	escm_atom_print(e, atom, stderr);	\
	fprintf(stderr, ": wrong argument.\n");	\
	(e)->err = -1;				\
	return NULL;				\
    }

/* Okay, this is ugly, but there is no other solution */
#define escm_assert1(test, atom, e, st)		\
    if (!(test)) {				\
	escm_atom_print(e, atom, stderr);	\
	fprintf(stderr, ": wrong argument.\n"); \
	st;					\
	(e)->err = -1;				\
	return NULL;				\
    }

struct escm_type {
    Escm_Fun_Mark fmark;
    Escm_Fun_Free ffree;
    Escm_Fun_Print fprint;
    Escm_Fun_Equal fequal;

    /* those two functions needs to be not null if only it add a new syntax to
       the parser */
    Escm_Fun_Parsetest fparsetest;
    Escm_Fun_Parse fparse;

    Escm_Fun_Eval feval;

    Escm_Fun_Exit fexit;
    void *dexit;
};

struct escm_context {
    escm_atom *first;
    escm_atom *last;

    escm_context *prev;

    unsigned int dotted : 1;
};

struct escm {
    escm_context *ctx;
    struct escm_slist *gard;

    escm_input *input;
    escm_atom *env;

    unsigned char **segments;
    escm_atom *freelist;
    escm_atom *heap;

    escm_atom *NIL; /* a NULL pair */
    escm_atom *EOF_OBJ; /* the EOF object */

    escm_atom *TRUE; /* the true boolean or a non null integer */
    escm_atom *FALSE; /* the false boolean or a null integer */

    escm_atom *curobj; /* the object we currently are treating */

    escm_type **types;
    size_t ntypes;

    int err;

    unsigned int casesensitive : 1;
    unsigned int quiet : 1;
};

escm *escm_new(void);
void escm_free(escm *);

void escm_fparse(escm *, const char *);
void escm_sparse(escm *, const char *);
escm_atom *escm_parse(escm *);

void escm_shell(escm *);

size_t escm_type_add(escm *, escm_type *);

void escm_ctx_enter(escm *);
void escm_ctx_put(escm *, escm_atom *);
void escm_ctx_put_splicing(escm *, escm_atom *);
escm_atom *escm_ctx_first(escm *);
escm_atom *escm_ctx_leave(escm *);
void escm_ctx_discard(escm *);

void escm_gc_collect(escm *);
void escm_gc_gard(escm *, escm_atom *);
void escm_gc_ungard(escm *, escm_atom *);

#endif /* ESCHEME_ESCM_H */
