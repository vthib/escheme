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
#ifndef ESCHEME_MACROS_H
# define ESCHEME_MACROS_H

#include "types.h"

#define ESCM_TYPE_MACRO escm_macro_tget()

#define ESCM_ISMACRO(x) ((x)->type == ESCM_TYPE_MACRO)

typedef struct escm_macro {
    escm_atom *literals;
    escm_atom *rules;
    escm_atom *env;
} escm_macro;

void escm_macros_init(escm *);
size_t escm_macro_tget(void);

escm_atom *escm_macro_expand(escm *, escm_atom *, escm_atom *);

escm_atom *escm_expand(escm *, escm_atom *);
escm_atom *escm_define_syntax(escm *, escm_atom *);
escm_atom *escm_syntax_rules(escm *, escm_atom *);

#endif /* ESCHEME_MACROS_H */
