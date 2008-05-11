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
#include <assert.h>

#include "escheme.h"

static inline void opaque(escm_atom *, escm_output *);
static inline void mark(escm *, size_t, void *);

escm_atom *
escm_atom_new(escm *e, size_t type, void *ptr)
{
    escm_atom *atom;

    assert(e != NULL);

    atom = e->freelist;
    if (!atom) { /* run the GC to collect some free atoms */
	/* before we mark the object we try to create */
	mark(e, type, ptr);
	escm_gc_collect(e);
	if (!e->freelist) {
	    fprintf(stderr, "memory is full.\n");
	    exit(EXIT_FAILURE);
	}
	atom = e->freelist;
    }
    e->freelist = atom->link;
    atom->link = e->heap, e->heap = atom;
    atom->type = type, atom->ptr = ptr;

    return atom;
}

void
escm_atom_free(escm *e, escm_atom *atom)
{
    escm_atom *link;

    assert(e != NULL);
    assert(atom != NULL);
    if (!atom->ptr)
	return;
    if (atom->type >= e->ntypes) {
	fprintf(stderr, "An atom have a unknown type.\n");
	e->err = 1;
	return;
    }

    if (e->types[atom->type]->ffree && atom->nofree == 0)
	e->types[atom->type]->ffree(atom->ptr);

    link = atom->link;
    memset(atom, 0, sizeof *atom);
    atom->link = link;
}

void
escm_atom_mark(escm *e, escm_atom *atom)
{
    assert(e != NULL);
    if (!atom || atom->marked == 1)
	return;
    if (atom->type >= e->ntypes) {
	fprintf(stderr, "An atom have a unknown type.\n");
	e->err = 1;
	return;
    }

    atom->marked = 1;
    mark(e, atom->type, atom->ptr);
}

escm_atom *
escm_atom_eval(escm *e, escm_atom *atom)
{
    escm_atom *ret, *old, *prevenv;

    assert(e != NULL);
    if (!atom)
	return NULL;
    if (atom->type >= e->ntypes) {
	fprintf(stderr, "An atom have a unknown type.\n");
	escm_abort(e);
    }

    old = e->curobj, e->curobj = atom;
    prevenv = (atom->env) ? escm_env_enter(e, atom->env) : NULL;

    if (e->types[atom->type]->type == TYPE_BUILT) {
	if (e->types[atom->type]->d.c.feval)
	    ret = e->types[atom->type]->d.c.feval(e, atom->ptr);
	else
	    ret = atom;
    } else {
	if (e->types[atom->type]->d.dyn.feval)
	    ret = escm_procedure_exec(e, e->types[atom->type]->d.dyn.feval,
				      escm_cons_make(e, atom, e->NIL), 0);
	else
	    ret = atom;
    }

    if (prevenv)
	escm_env_leave(e, prevenv);
    e->curobj = old;

    return ret;
}

escm_atom *
escm_atom_exec(escm *e, escm_atom *atom, escm_atom *args)
{
    escm_atom *ret, *old;

    assert(e != NULL);
    if (!atom)
	return NULL;
    if (atom->type >= e->ntypes) {
	fprintf(stderr, "An atom have a unknown type.\n");
	escm_abort(e);
    }

    old = e->curobj, e->curobj = atom;
    if (e->types[atom->type]->type == TYPE_BUILT) {
	if (e->types[atom->type]->d.c.fexec)
	    ret = e->types[atom->type]->d.c.fexec(e, atom->ptr, args);
	else
	    goto noexec;
    } else {
	if (e->types[atom->type]->d.dyn.fexec)
	    ret = escm_procedure_exec(e, e->types[atom->type]->d.dyn.fexec,
				      escm_cons_make(e, atom, args), 0);
	else
	    goto noexec;
    }
    e->curobj = old;

    return ret;

noexec:
    escm_error(e, "~s: object isn't applicable.~%", atom);
    escm_abort(e);
}


void
escm_atom_print4(escm *e, escm_atom *atom, escm_output *stream, int lvl)
{
    escm_atom *old;

    assert(e != NULL);
    if (!atom)
	return;
    if (atom->type >= e->ntypes) {
	fprintf(stderr, "An atom have a unknown type.\n");
	e->err = 1;
	return;
    }

    old = e->curobj, e->curobj = atom;
    if (e->types[atom->type]->type == TYPE_BUILT) {
	if (!e->types[atom->type]->d.c.fprint)
	    opaque(atom, stream);
	else
	    e->types[atom->type]->d.c.fprint(e, atom->ptr, stream, lvl);
#if defined ESCM_USE_PORTS && defined ESCM_USE_NUMBERS
    } else {
	if (!e->types[atom->type]->d.dyn.fprint)
	    opaque(atom, stream);
	else {
	    escm_atom *a;

	    escm_ctx_enter(e);
	    escm_ctx_put(e, atom);
	    a = escm_port_make(e, stream, 0);
	    escm_port_val(a)->nofree = 1;
	    escm_ctx_put(e, a);
	    escm_ctx_put(e, escm_int_make(e, (long) lvl));

	    (void) escm_procedure_exec(e, e->types[atom->type]->d.dyn.fprint,
				       escm_ctx_leave(e), 0);
	}
#endif
    }
    e->curobj = old;
}

int
escm_atom_equal(escm *e, escm_atom *o1, escm_atom *o2, int lvl)
{
    assert(e != NULL);

    if (!o1 || !o2 || o1->type != o2->type)
	return 0;
    if (o1->type >= e->ntypes) {
	fprintf(stderr, "An atom have a unknown type.\n");
	e->err = 1;
	return 0;
    }
    if (o1->ptr == o2->ptr)
	return 1;

    if (e->types[o1->type]->type == TYPE_DYN) {
	escm_atom *a;

	if (!e->types[o1->type]->d.dyn.fequal)
	    return 0;
	a = escm_cons_make(e, o1, escm_cons_make(e, o2, e->NIL));
	a = escm_procedure_exec(e, e->types[o1->type]->d.dyn.fequal, a, 0);
	return ESCM_ISTRUE(a) ? 1 : 0;
    } else {
	if (!e->types[o1->type]->d.c.fequal)
	    return 0;
	return e->types[o1->type]->d.c.fequal(e, o1->ptr, o2->ptr, lvl);
    }
}

static inline void
mark(escm *e, size_t type, void *ptr)
{
    assert(e != NULL);

    if (e->types[type]->fmark)
	e->types[type]->fmark(e, ptr);
}

static inline void
opaque(escm_atom *atom, escm_output *output)
{
    escm_printf(output, "#<opaque-type %lu>", atom->type);
}
