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
#ifndef ESCHEME_TYPE_H
# define ESCHEME_TYPE_H

#include "types.h"

void escm_type_init(escm *);

escm_atom *escm_get_type(escm *, escm_atom *);
escm_atom *escm_create_type(escm *, escm_atom *);
escm_atom *escm_set_type_x(escm *, escm_atom *);
escm_atom *escm_type_p(escm *, escm_atom *);
escm_atom *escm_rep(escm *, escm_atom *);

escm_atom *escm_set_print(escm *, escm_atom *);
escm_atom *escm_set_eval(escm *, escm_atom *);
escm_atom *escm_set_equal(escm *, escm_atom *);
escm_atom *escm_set_parse_p(escm *, escm_atom *);
escm_atom *escm_set_parse(escm *, escm_atom *);

#endif /* ESCHEME_TYPE_H */