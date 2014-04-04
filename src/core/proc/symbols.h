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
#ifndef ESCHEME_PROC_SYMBOLS_H
# define ESCHEME_PROC_SYMBOLS_H

#include "types.h"

void escm_addprims_symbol(escm *);

escm_atom *escm_symbol_p(escm *, escm_atom *, void *);

escm_atom *escm_lookup(escm *, escm_atom *, void *);

#endif /* ESCHEME_PROC_SYMBOLS_H */
