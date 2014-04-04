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
#ifndef ESCHEME_PROC_PROCEDURES_H
# define ESCHEME_PROC_PROCEDURES_H

#include "types.h"

void escm_addprims_procedure(escm *);

escm_atom *escm_procedure_p(escm *, escm_atom *, void *);
escm_atom *escm_apply(escm *, escm_atom *, void *);
escm_atom *escm_map(escm *, escm_atom *, void *);
escm_atom *escm_for_each(escm *, escm_atom *, void *);

#endif /* ESCHEME_PROC_PROCEDURES_H */
