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
#ifndef ESCHEME_USTRINGS_H
# define ESCHEME_USTRINGS_H

#include "types.h"

#define ESCM_TYPE_USTRING escm_ustring_tget()

#define ESCM_ISUSTR(x) ((x)->type == ESCM_TYPE_USTRING)

#define escm_ustr_val(x) (((escm_ustring *) (x)->ptr)->str)
#define escm_ustr_len(x) (((escm_ustring *) (x)->ptr)->len)

typedef struct escm_ustring {
    wchar_t *str;
    size_t len;
} escm_ustring;

void escm_ustrings_init(escm *);
size_t escm_ustring_tget(void);
escm_atom *escm_ustring_make(escm *, const wchar_t *, size_t);

escm_atom *escm_ustring_p(escm *, escm_atom *);
escm_atom *escm_make_ustring(escm *, escm_atom *);
escm_atom *escm_prim_ustring(escm *, escm_atom *);
escm_atom *escm_ustring_length(escm *, escm_atom *);
escm_atom *escm_ustring_ref(escm *, escm_atom *);
escm_atom *escm_ustring_set_x(escm *, escm_atom *);

escm_atom *escm_ustring_eq_p(escm *, escm_atom *);
escm_atom *escm_ustring_lt_p(escm *, escm_atom *);
escm_atom *escm_ustring_gt_p(escm *, escm_atom *);
escm_atom *escm_ustring_le_p(escm *, escm_atom *);
escm_atom *escm_ustring_ge_p(escm *, escm_atom *);

escm_atom *escm_ustring_ci_eq_p(escm *, escm_atom *);
escm_atom *escm_ustring_ci_lt_p(escm *, escm_atom *);
escm_atom *escm_ustring_ci_gt_p(escm *, escm_atom *);
escm_atom *escm_ustring_ci_le_p(escm *, escm_atom *);
escm_atom *escm_ustring_ci_ge_p(escm *, escm_atom *);

escm_atom *escm_subustring(escm *, escm_atom *);
escm_atom *escm_ustring_append(escm *, escm_atom *);
escm_atom *escm_ustring_copy(escm *, escm_atom *);
escm_atom *escm_ustring_fill_x(escm *, escm_atom *);

escm_atom *escm_ustring_to_list(escm *, escm_atom *);
escm_atom *escm_list_to_ustring(escm *, escm_atom *);

#endif /* ESCHEME_USTRINGS_H */
