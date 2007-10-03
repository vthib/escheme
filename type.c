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
#include "escheme.h"

void
escm_type_init(escm *e)
{
    (void) escm_procedure_new(e, "get-type", 1, 1, escm_get_type, NULL);
    (void) escm_procedure_new(e, "create-type", 1, 1, escm_create_type, NULL);
    (void) escm_procedure_new(e, "set-type!", 2, 2, escm_set_type_x, NULL);
    (void) escm_procedure_new(e, "type?", 2, 2, escm_type_p, NULL);
    (void) escm_procedure_new(e, "rep", 1, 1, escm_rep, NULL);

    (void) escm_procedure_new(e, "set-print", 2, 2, escm_set_print, NULL);
    (void) escm_procedure_new(e, "set-eval", 2, 2, escm_set_eval, NULL);
    (void) escm_procedure_new(e, "set-equal", 2, 2, escm_set_equal, NULL);
    (void) escm_procedure_new(e, "set-parse?", 2, 2, escm_set_parse, NULL);
    (void) escm_procedure_new(e, "set-parse", 2, 2, escm_set_parse, NULL);
}


escm_atom *
escm_get_type(escm *e, escm_atom *args)
{
    return escm_int_make(e, escm_cons_car(args)->type);
}

escm_atom *
escm_create_type(escm *e, escm_atom *args)
{
    escm_atom *basetype;
    unsigned long i;
    escm_type *t;

    basetype = escm_cons_pop(e, &args);
    if (escm_number_ival(basetype) < 0 ||
	(unsigned long) escm_number_ival(basetype) >= e->ntypes) {
	escm_error(e, "create-type: ~s is not a type.~%", basetype);
	escm_abort(e);
    }

    i = (unsigned long) escm_number_ival(basetype);
    t = xcalloc(1, sizeof *t);
    t->type = TYPE_DYN;
    t->d.dyn.basetype = i;
    t->fmark = e->types[i]->fmark;
    t->ffree = e->types[i]->ffree;

    return escm_int_make(e, (long) escm_type_add(e, t));
}

escm_atom *
escm_set_type_x(escm *e, escm_atom *args)
{
    escm_atom *type, *atom;
    long t;

    atom = escm_cons_pop(e, &args);
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    t = escm_number_ival(type);
    if (t < 0 || (unsigned long) t >= e->ntypes) {
	escm_error(e, "set-type!: ~s is not a type.~%", type);
	escm_abort(e);
    }

    if (e->types[t]->type != TYPE_DYN) {
	fprintf(stderr, "set-type!: can only set to a dynamic type.\n");
	escm_abort(e);
    }

    if (atom->type != e->types[t]->d.dyn.basetype) {
	fprintf(stderr, "Trying to create differents atoms with same type. "
		"Aborting.\n");
	escm_abort(e);
    }

    atom->type = (unsigned long) t;
    return NULL;
}

escm_atom *
escm_type_p(escm *e, escm_atom *args)
{
    escm_atom *type, *atom;

    atom = escm_cons_pop(e, &args);
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "type?: ~s is not a type.~%", type);
	escm_abort(e);
    }

    return ((unsigned long) escm_number_ival(type) == atom->type) ? e->TRUE :
	e->FALSE;
}

escm_atom *
escm_rep(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    if (e->types[atom->type]->type != TYPE_DYN) {
	escm_error(e, "rep: expect an argument of dynamic type, not ~s.~%",
		   atom);
	escm_abort(e);
    }

    atom = escm_atom_new(e, e->types[atom->type]->d.dyn.basetype, atom->ptr);
    atom->nofree = 1;
    return atom;
}

escm_atom *
escm_set_print(escm *e, escm_atom *args)
{
    escm_atom *type, *proc;

    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "set-print: ~s is not a type.~%", type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	fprintf(stderr, "set-print: given type must be a dynamic type.\n");
	escm_abort(e);
    }

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->d.dyn.fprint = proc;
    return NULL;
}

escm_atom *
escm_set_eval(escm *e, escm_atom *args)
{
    escm_atom *type, *proc;

    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "set-eval: ~s is not a type.~%", type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	fprintf(stderr, "set-eval: given type must be a dynamic type.\n");
	escm_abort(e);
    }

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->d.dyn.feval = proc;
    return NULL;
}

escm_atom *
escm_set_equal(escm *e, escm_atom *args)
{
    escm_atom *type, *proc;

    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "set-equal: ~s is not a type.~%", type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	fprintf(stderr, "set-equal: given type must be a dynamic type.\n");
	escm_abort(e);
    }

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->d.dyn.fequal = proc;
    return NULL;
}

escm_atom *
escm_set_parse_p(escm *e, escm_atom *args)
{
    escm_atom *type, *proc;

    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "set-parse?: ~s is not a type.~%", type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	fprintf(stderr, "set-parse?: given type must be a dynamic type.\n");
	escm_abort(e);
    }

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->d.dyn.fparsetest = proc;
    return NULL;
}

escm_atom *
escm_set_parse(escm *e, escm_atom *args)
{
    escm_atom *type, *proc;

    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "set-parse: ~s is not a type.~%", type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	fprintf(stderr, "set-parse: given type must be a dynamic type.\n");
	escm_abort(e);
    }

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->d.dyn.fparse = proc;
    return NULL;
}

