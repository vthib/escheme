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

#include "symbols.h"
#include "escm.h"
#include "atom.h"

#include "type/symbols.h"
#include "type/procedures.h"
#include "type/cons.h"

void
escm_addprims_symbol(escm *e)
{
    escm_atom *a;

    (void) escm_procedure_new(e, T("symbol?"), 1, 1, escm_symbol_p, NULL);

    a = escm_procedure_new(e, T("lookup"), 1, 1, escm_lookup, NULL);
    escm_proc_val(a)->d.c.quoted = 0x1;
}

escm_atom *
escm_symbol_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    return ESCM_ISSYM(a) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_lookup(escm *e, escm_atom *args, void *nil)
{
    escm_atom *sym;

    (void) nil;
    sym = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(sym), sym, e);

    return (escm_sym_val(sym) != NULL) ? escm_sym_val(sym) : e->FALSE;
}
