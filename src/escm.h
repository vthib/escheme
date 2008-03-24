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
#include <setjmp.h>
#include "types.h"
#include "cons.h"

#define escm_fun(e) ((e)->ctx->fun)

#define escm_abort(e) do { (e)->err = 1; return NULL; } while(0)

#define escm_assert(test, atom, e)					\
    do {								\
	if (!(test)) {							\
	    escm_error((e), "~s: ~s: wrong argument.~%", escm_fun(e), (atom)); \
	    escm_abort(e);						\
	}								\
    } while(0)

/* Okay, this is ugly, but there is no other solution */
#define escm_assert1(test, atom, e, st)					\
    do {								\
	if (!(test)) {							\
	    escm_error((e), "~s: ~s: wrong argument.~%", escm_fun(e), (atom)); \
	    st;								\
	    escm_abort(e);						\
	}								\
    } while(0)

enum { TYPE_BUILT, TYPE_DYN };

struct escm_type {
    Escm_Fun_Mark fmark;
    Escm_Fun_Free ffree;

    union {
	struct {
	    Escm_Fun_Print fprint;
	    Escm_Fun_Equal fequal;

	    /* those two functions needs to be not null if only it add a new
	       syntax to the parser */
	    Escm_Fun_Parsetest fparsetest;
	    Escm_Fun_Parse fparse;

	    Escm_Fun_Eval feval;
	    Escm_Fun_Exec fexec;

	    Escm_Fun_Exit fexit;
	    void *dexit;
	} c;
	struct {
	    escm_atom *fprint;
	    escm_atom *fequal;
	    escm_atom *fparsetest;
	    escm_atom *fparse;
	    escm_atom *feval;
	    escm_atom *fexec;

	    unsigned long basetype;
	} dyn;
    } d;

    unsigned int type : 1;
};

struct escm_context {
    jmp_buf jbuf;
    escm_atom *fun;

    escm_atom *first;
    escm_atom *last;

    escm_context *prev;

    unsigned int dotted : 1;
};

struct escm {
    escm_context *ctx;
    struct escm_slist *gard;

    escm_input *input;
    escm_output *output;
    escm_output *errp;

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
    unsigned long ntypes;

    unsigned int backtrace : 1;
    unsigned int casesensitive : 1;
    unsigned int brackets : 1;
    unsigned int tailrec : 1;
    unsigned int err : 1;
};

escm *escm_new(void);
void escm_free(escm *);

int escm_fparse(escm *, const char *);
int escm_sparse(escm *, const char *);
escm_atom *escm_parse(escm *);

void escm_shell(escm *);

#define escm_type_ison(type) ((type) != 0)
unsigned long escm_type_add(escm *, escm_type *);

void escm_ctx_enter(escm *);
void escm_ctx_put(escm *, escm_atom *);
void escm_ctx_put_splicing(escm *, escm_atom *);
escm_atom *escm_ctx_first(escm *);
escm_atom *escm_ctx_leave(escm *);
void escm_ctx_discard(escm *);

void escm_gc_collect(escm *);
void escm_gc_gard(escm *, escm_atom *);
void escm_gc_ungard(escm *, escm_atom *);

void escm_tailrec(escm *, escm_atom *, int);
void escm_tailrec3(escm *, escm_atom *, escm_atom *, int);

void escm_print_backtrace(escm *, escm_output *);

#endif /* ESCHEME_ESCM_H */
