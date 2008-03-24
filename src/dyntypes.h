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
#ifndef ESCHEME_DYNTYPES_H
# define ESCHEME_DYNTYPES_H

#include "types.h"

void escm_dyntypes_init(escm *);

escm_atom *escm_get_type(escm *, escm_atom *);
escm_atom *escm_make_new_type(escm *, escm_atom *);
escm_atom *escm_rep_to_data(escm *, escm_atom *);
escm_atom *escm_data_to_rep(escm *, escm_atom *);
escm_atom *escm_of_type_p(escm *, escm_atom *);

escm_atom *escm_set_print(escm *, escm_atom *);
escm_atom *escm_set_eval(escm *, escm_atom *);
escm_atom *escm_set_equal(escm *, escm_atom *);
escm_atom *escm_set_parse_p(escm *, escm_atom *);
escm_atom *escm_set_parse(escm *, escm_atom *);

#endif /* ESCHEME_DYNTYPES_H */