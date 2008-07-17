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
escm_dyntypes_init(escm *e)
{
    (void) escm_procedure_new(e, "type", 1, 1, escm_prim_type, NULL);
    (void) escm_procedure_new(e, "create-type", 4, 4, escm_create_type,
			      NULL);

    (void) escm_procedure_new(e, "set-print", 2, 2, escm_set_print, NULL);
    (void) escm_procedure_new(e, "set-eval", 2, 2, escm_set_eval, NULL);
    (void) escm_procedure_new(e, "set-equal", 2, 2, escm_set_equal, NULL);
    (void) escm_procedure_new(e, "set-parse?", 2, 2, escm_set_parse_p, NULL);
    (void) escm_procedure_new(e, "set-parse", 2, 2, escm_set_parse, NULL);
}


escm_atom *
escm_prim_type(escm *e, escm_atom *args)
{
    return escm_int_make(e, escm_cons_car(args)->type);
}

escm_atom *
escm_create_type(escm *e, escm_atom *args)
{
    escm_type *t;
    escm_atom *accessor, *constructor, *pred, *basetype;
    long i;

    constructor = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(constructor), constructor, e);
    accessor = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(accessor), accessor, e);
    pred = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(pred), pred, e);
    basetype = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(basetype), basetype, e);
    i = escm_number_ival(basetype);
    if (i < 0 || ((unsigned long) i) >= e->ntypes) {
	escm_error(e, "~s is not a proper type number.~%", basetype);
	escm_abort(e);
    }

    t = xcalloc(1, sizeof *t);
    t->type = TYPE_DYN;
    t->d.dyn.basetype = i;
    t->fmark = (Escm_Fun_Mark) escm_atom_mark;
 
    i = (long) escm_type_add(e, t);

    (void) escm_procedure_new(e, escm_sym_name(constructor), 1, 1,
			      (Escm_Fun_Prim) escm_rep_to_data,
			      (void *) &e->types[i]);
    (void) escm_procedure_new(e, escm_sym_name(accessor), 1, 1,
			      escm_data_to_rep, NULL);
    (void) escm_procedure_new(e, escm_sym_name(pred), 1, 1,
			      (Escm_Fun_Prim) escm_of_type_p,
			      (void *) &e->types[i]);

    return escm_int_make(e, i);
}

escm_atom *
escm_rep_to_data(escm *e, escm_atom *args, escm_type **type)
{
    escm_atom *atom, *a;

    atom = escm_cons_pop(e, &args);

    if (atom->type != (*type)->d.dyn.basetype) {
	escm_error(e, "~s: can't create differents atoms with same type.~%",
		   escm_fun(e));
	escm_abort(e);
    }

    a = escm_atom_new(e, type - e->types, atom);
    a->nofree = 1;
    return a;
}

escm_atom *
escm_data_to_rep(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    if (e->types[atom->type]->type != TYPE_DYN) {
	escm_error(e, "~s: expect an argument of dynamic type, not ~s.~%",
		   escm_fun(e), atom);
	escm_abort(e);
    }

    return atom->ptr;
}

escm_atom *
escm_of_type_p(escm *e, escm_atom *args, escm_type **type)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);

    return ((unsigned long) (type - e->types) == atom->type) ? e->TRUE :
	e->FALSE;
}

escm_atom *
escm_set_print(escm *e, escm_atom *args)
{
    escm_atom *type, *proc;

    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "~s: ~s is not a type.~%", escm_fun(e), type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	escm_error(e, "~s: given type must be a dynamic type.~%", escm_fun(e));
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
	escm_error(e, "~s: ~s is not a type.~%", escm_fun(e), type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	escm_error(e, "~s: given type must be a dynamic type.~%", escm_fun(e));
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
	escm_error(e, "~s: ~s is not a type.~%", escm_fun(e), type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	escm_error(e, "~s: given type must be a dynamic type.~%", escm_fun(e));
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
	escm_error(e, "~s: ~s is not a type.~%", escm_fun(e), type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	escm_error(e, "~s: given type must be a dynamic type.~%", escm_fun(e));
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
	escm_error(e, "~s: ~s is not a type.~%", escm_fun(e), type);
	escm_abort(e);
    }
    if (e->types[escm_number_ival(type)]->type != TYPE_DYN) {
	escm_error(e, "~s: given type must be a dynamic type.~%", escm_fun(e));
	escm_abort(e);
    }

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->d.dyn.fparse = proc;
    return NULL;
}
