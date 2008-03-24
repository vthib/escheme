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
    (void) escm_procedure_new(e, "get-type", 1, 1, escm_get_type, NULL);
    (void) escm_procedure_new(e, "make-new-type", 0, 0, escm_make_new_type,
			      NULL);
    (void) escm_procedure_new(e, "rep->data", 2, 2, escm_rep_to_data, NULL);
    (void) escm_procedure_new(e, "data->rep", 1, 1, escm_data_to_rep, NULL);
    (void) escm_procedure_new(e, "of-type?", 2, 2, escm_of_type_p, NULL);

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
escm_make_new_type(escm *e, escm_atom *args)
{
    escm_type *t;

    (void) args;

    t = xcalloc(1, sizeof *t);
    t->type = TYPE_DYN;

    return escm_int_make(e, (long) escm_type_add(e, t));
}

escm_atom *
escm_rep_to_data(escm *e, escm_atom *args)
{
    escm_atom *type, *atom, *a;
    long t;

    atom = escm_cons_pop(e, &args);
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    t = escm_number_ival(type);
    if (t < 0 || (unsigned long) t >= e->ntypes) {
	escm_error(e, "~s: ~s is not a type.~%", escm_fun(e), type);
	escm_abort(e);
    }

    if (e->types[t]->type != TYPE_DYN) {
	escm_error(e, "~s: can only set to a dynamic type.~%", escm_fun(e));
	escm_abort(e);
    }

    if (e->types[t]->fmark == NULL) { /* basetype not set yet */
	e->types[t]->d.dyn.basetype = atom->type;
	e->types[t]->fmark = (Escm_Fun_Mark) escm_atom_mark;
	e->types[t]->ffree = NULL;
    } else if (atom->type != e->types[t]->d.dyn.basetype) {
	escm_error(e, "~s: can't create differents atoms with same type.~%",
		   escm_fun(e));
	escm_abort(e);
    }

    a = escm_atom_new(e, t, atom);
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
escm_of_type_p(escm *e, escm_atom *args)
{
    escm_atom *type, *atom;

    atom = escm_cons_pop(e, &args);
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(type), type, e);
    if (escm_number_ival(type) < 0 ||
	(unsigned long) escm_number_ival(type) >= e->ntypes) {
	escm_error(e, "~s: ~s is not a type.~%", escm_fun(e), type);
	escm_abort(e);
    }

    return ((unsigned long) escm_number_ival(type) == atom->type) ? e->TRUE :
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
