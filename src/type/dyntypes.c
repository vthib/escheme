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

#include "dyntypes.h"
#include "base.h"
#include "numbers.h"
#include "chars.h"

void
escm_dyntypes_init(escm *e)
{
    (void) escm_procedure_new(e, T("type"), 1, 1, escm_prim_type, NULL);
    (void) escm_procedure_new(e, T("create-type"), 4, 4, escm_create_type,
                              NULL);

    (void) escm_procedure_new(e, T("set-print"), 2, 2, escm_set_print, NULL);
    (void) escm_procedure_new(e, T("set-equal"), 2, 2, escm_set_equal, NULL);
    (void) escm_procedure_new(e, T("set-parse?"), 2, 2, escm_set_parse_p, NULL);
    (void) escm_procedure_new(e, T("set-parse"), 2, 2, escm_set_parse, NULL);
    (void) escm_procedure_new(e, T("set-eval"), 2, 2, escm_set_eval, NULL);
    (void) escm_procedure_new(e, T("set-exec"), 2, 2, escm_set_exec, NULL);

    (void) escm_procedure_new(e, T("type-parse?"), 2, 2, escm_prim_type_parse_p,
                              NULL);
    (void) escm_procedure_new(e, T("type-parse"), 1, 1, escm_prim_type_parse,
                              NULL);
}

escm_atom *
escm_prim_type(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return escm_int_make(e, escm_cons_car(args)->type);
}

escm_atom *
escm_create_type(escm *e, escm_atom *args, void *nil)
{
    escm_type *t;
    escm_atom *accessor, *constructor, *pred, *basetype;
    long i;

    (void) nil;
    constructor = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(constructor), constructor, e);
    accessor = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(accessor), accessor, e);
    pred = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(pred), pred, e);
    basetype = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, basetype), basetype, e);

    t = xcalloc(1, sizeof *t);
    t->dtype = TYPE_DYN;
    t->d.dyn.basetype = escm_number_ival(basetype);
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
        escm_error(e, _(T("~s: ~s type and basetype for this dynamic type "))
                   "mismatch.~%", escm_fun(e), atom);
        escm_abort(e);
    }

    a = escm_atom_new(e, type - e->types, atom);
    a->nofree = 1;
    return a;
}

escm_atom *
escm_data_to_rep(escm *e, escm_atom *args, void *nil)
{
    escm_atom *atom;

    (void) nil;
    atom = escm_cons_pop(e, &args);
    if (e->types[atom->type]->dtype != TYPE_DYN) {
        escm_error(e, _(T("~s: expect an argument of dynamic type, not ~s.~%")),
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

    return ((size_t) (type - e->types) == atom->type) ? e->TRUE :
        e->FALSE;
}

escm_atom *
escm_set_print(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type, *proc;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->print.pprint = proc;
    e->types[escm_number_ival(type)]->printtype = TYPE_DYN;
    return NULL;
}

escm_atom *
escm_set_equal(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type, *proc;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->equal.pequal = proc;
    e->types[escm_number_ival(type)]->equaltype = TYPE_DYN;
    return NULL;
}

escm_atom *
escm_set_parse_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type, *proc;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->parsetest.pparsetest = proc;
    e->types[escm_number_ival(type)]->parsetesttype = TYPE_DYN;
    return NULL;
}

escm_atom *
escm_set_parse(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type, *proc;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->parse.pparse = proc;
    e->types[escm_number_ival(type)]->parsetype = TYPE_DYN;
    return NULL;
}

escm_atom *
escm_set_eval(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type, *proc;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->eval.peval = proc;
    e->types[escm_number_ival(type)]->evaltype = TYPE_DYN;
    return NULL;
}

escm_atom *
escm_set_exec(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type, *proc;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    proc = escm_cons_pop(e, &args);
    e->types[escm_number_ival(type)]->exec.pexec = proc;
    e->types[escm_number_ival(type)]->exectype = TYPE_DYN;
    return NULL;
}

escm_atom *
escm_prim_type_parse_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type, *character;
    int c;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    character = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(character), character, e);

    c = escm_input_getc(e->input);
    if (c != '\n')
        escm_input_ungetc(e->input, c);

    return escm_type_parsetest(e, (size_t) escm_number_ival(type),
                               e->input, escm_char_val(character))
        ? e->TRUE : e->FALSE;
}

escm_atom *
escm_prim_type_parse(escm *e, escm_atom *args, void *nil)
{
    escm_atom *type;
    int c;

    (void) nil;
    type = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISTYPE(e, type), type, e);

    c = escm_input_getc(e->input);
    if (c != '\n')
        escm_input_ungetc(e->input, c);

    return escm_type_parse(e, (size_t) escm_number_ival(type), e->input);
}
