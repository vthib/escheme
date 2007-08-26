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
#ifndef ESCHEME_SYMBOLS_H
# define ESCHEME_SYMBOLS_H

#include "types.h"

#define ESCM_TYPE_SYMBOL escm_symbol_tget()

#define ESCM_ISSYM(x) ((x)->type == ESCM_TYPE_SYMBOL)

#define ESCM_SYM_VAL(x) ((const char *) (x)->ptr)

void escm_symbols_init(escm *);
size_t escm_symbol_tget(void);
escm_atom *escm_symbol_make(escm *, const char *);

escm_atom *escm_symbol_p(escm *, escm_atom *);
escm_atom *escm_symbol_to_string(escm *, escm_atom *);
escm_atom *escm_string_to_symbol(escm *, escm_atom *);

#endif /* ESCHEME_SYMBOLS_H */
