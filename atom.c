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

    if (e->types[atom->type]->ffree)
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

    atom->marked = 1;
    mark(e, atom->type, atom->ptr);
}

escm_atom *
escm_atom_eval(escm *e, escm_atom *atom)
{
    escm_atom *ret, *old;

    assert(e != NULL);
    if (!atom)
	return NULL;

    old = e->curobj, e->curobj = atom;
    if (e->types[atom->type]->feval)
	ret = e->types[atom->type]->feval(e, atom->ptr);
    else
	ret = atom;
    e->curobj = old;
    return ret;
}

void
escm_atom_print4(escm *e, escm_atom *atom, escm_output *stream, int lvl)
{
    escm_atom *old;

    assert(e != NULL);
    if (!atom)
	return;

    old = e->curobj, e->curobj = atom;
    e->types[atom->type]->fprint(e, atom->ptr, stream, lvl);
    e->curobj = old;
}

int
escm_atom_equal(escm *e, escm_atom *o1, escm_atom *o2, int lvl)
{
    assert(e != NULL);
    if (!o1 || !o2 || o1->type != o2->type || !e->types[o1->type]->fequal)
	return 0;
    if (o1->ptr == o2->ptr)
	return 1;

    return e->types[o1->type]->fequal(e, o1->ptr, o2->ptr, lvl);
}

static inline void
mark(escm *e, size_t type, void *ptr)
{
    assert(e != NULL);

    if (e->types[type]->fmark)
	e->types[type]->fmark(e, ptr);
}
