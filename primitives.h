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
#ifndef ESCHEME_PRIMITIVES_H
# define ESCHEME_PRIMITIVES_H

#include "types.h"

void escm_primitives_load(escm *);

escm_atom *escm_quote(escm *, escm_atom *);
escm_atom *escm_lambda(escm *, escm_atom *);

escm_atom *escm_define(escm *, escm_atom *);
escm_atom *escm_set_x(escm *, escm_atom *);

escm_atom *escm_let(escm *, escm_atom *);
escm_atom *escm_let_star(escm *, escm_atom *);
escm_atom *escm_letrec(escm *, escm_atom *);

escm_atom *escm_if(escm *, escm_atom *);
escm_atom *escm_cond(escm *, escm_atom *);
escm_atom *escm_case(escm *, escm_atom *);

escm_atom *escm_and(escm *, escm_atom *);
escm_atom *escm_or(escm *, escm_atom *);

escm_atom *escm_begin(escm *, escm_atom *);
escm_atom *escm_do(escm *, escm_atom *);

escm_atom *escm_eqv_p(escm *, escm_atom *);
escm_atom *escm_eq_p(escm *, escm_atom *);
escm_atom *escm_equal_p(escm *, escm_atom *);

#endif /* ESCHEME_PRIMITIVES_H */
