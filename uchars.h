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
#ifndef ESCHEME_UCHARS_H
# define ESCHEME_UCHARS_H

#include "types.h"

#define ESCM_TYPE_UCHAR escm_uchar_tget()

#define ESCM_ISUCHAR(x) ((x)->type == ESCM_TYPE_UCHAR)

#define escm_uchar_val(x) ((wchar_t) (escm_intptr) (x)->ptr)

void escm_uchars_init(escm *);
size_t escm_uchar_tget(void);
escm_atom *escm_uchar_make(escm *, wchar_t);

escm_atom *escm_uchar_p(escm *, escm_atom *);

escm_atom *escm_uchar_eq_p(escm *, escm_atom *);
escm_atom *escm_uchar_lt_p(escm *, escm_atom *);
escm_atom *escm_uchar_gt_p(escm *, escm_atom *);
escm_atom *escm_uchar_le_p(escm *, escm_atom *);
escm_atom *escm_uchar_ge_p(escm *, escm_atom *);

escm_atom *escm_uchar_ci_eq_p(escm *, escm_atom *);
escm_atom *escm_uchar_ci_lt_p(escm *, escm_atom *);
escm_atom *escm_uchar_ci_gt_p(escm *, escm_atom *);
escm_atom *escm_uchar_ci_le_p(escm *, escm_atom *);
escm_atom *escm_uchar_ci_ge_p(escm *, escm_atom *);

escm_atom *escm_uchar_alphabetic_p(escm *, escm_atom *);
escm_atom *escm_uchar_numeric_p(escm *, escm_atom *);
escm_atom *escm_uchar_whitespace_p(escm *, escm_atom *);
escm_atom *escm_uchar_upper_case_p(escm *, escm_atom *);
escm_atom *escm_uchar_lower_case_p(escm *, escm_atom *);

escm_atom *escm_uchar_to_integer(escm *, escm_atom *);
escm_atom *escm_integer_to_uchar(escm *, escm_atom *);

escm_atom *escm_uchar_upcase(escm *, escm_atom *);
escm_atom *escm_uchar_downcase(escm *, escm_atom *);

#endif /* ESCHEME_UCHARS_H */
