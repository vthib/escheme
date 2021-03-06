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
#ifndef ESCHEME_BOOLEANS_H
# define ESCHEME_BOOLEANS_H

#ifdef ESCM_USE_BOOLEANS

# include "types.h"

# define ESCM_TYPE_BOOLEAN escm_boolean_tget()

# define ESCM_ISBOOL(x) ((x)->type == ESCM_TYPE_BOOLEAN)

void escm_booleans_init(escm *);

escm_atom *escm_not(escm *, escm_atom *, void *);
escm_atom *escm_boolean_p(escm *, escm_atom *, void *);

size_t escm_boolean_tget(void);

#endif /* ESCM_USE_NUMBERS */

#endif /* ESCHEME_BOOLEANS_H */
