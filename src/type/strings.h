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
#ifndef ESCHEME_STRINGS_H
# define ESCHEME_STRINGS_H

#ifdef ESCM_USE_STRINGS

#include "types.h"

#define ESCM_TYPE_STRING escm_string_tget()

#define ESCM_ISSTR(x) ((x)->type == ESCM_TYPE_STRING)

#define escm_str_val(x) (((escm_string *) (x)->ptr)->str)
#define escm_str_len(x) (((escm_string *) (x)->ptr)->len)

typedef struct escm_string {
    tchar *str;
    size_t len;
} escm_string;

void escm_strings_init(escm *);
size_t escm_string_tget(void);
escm_atom *escm_string_make(escm *, const tchar *, size_t);
escm_atom *escm_string_make2(escm *, const tchar *);

escm_atom *escm_string_p(escm *, escm_atom *, void *);
escm_atom *escm_make_string(escm *, escm_atom *, void *);
escm_atom *escm_prim_string(escm *, escm_atom *, void *);
escm_atom *escm_string_length(escm *, escm_atom *, void *);
escm_atom *escm_string_ref(escm *, escm_atom *, void *);
escm_atom *escm_string_set_x(escm *, escm_atom *, void *);

escm_atom *escm_string_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_string_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_string_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_string_le_p(escm *, escm_atom *, void *);
escm_atom *escm_string_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_string_ci_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_string_ci_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_string_ci_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_string_ci_le_p(escm *, escm_atom *, void *);
escm_atom *escm_string_ci_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_substring(escm *, escm_atom *, void *);
escm_atom *escm_string_append(escm *, escm_atom *, void *);
escm_atom *escm_string_copy(escm *, escm_atom *, void *);
escm_atom *escm_string_fill_x(escm *, escm_atom *, void *);

escm_atom *escm_string_to_list(escm *, escm_atom *, void *);
escm_atom *escm_list_to_string(escm *, escm_atom *, void *);

escm_atom *escm_symbol_to_string(escm *, escm_atom *, void *);
escm_atom *escm_string_to_symbol(escm *, escm_atom *, void *);

# ifdef ESCM_USE_NUMBERS
escm_atom *escm_number_to_string(escm *, escm_atom *, void *);
escm_atom *escm_string_to_number(escm *, escm_atom *, void *);
# endif /* ESCM_USE_NUMBERS */

#endif /* ESCM_USE_STRINGS */

#endif /* ESCHEME_STRINGS_H */
