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
#ifndef ESCHEME_ACHARS_H
# define ESCHEME_ACHARS_H

#include "types.h"

#define ESCM_TYPE_ACHAR escm_achar_tget()

#define ESCM_ISACHAR(x) ((x)->type == ESCM_TYPE_ACHAR)

#define escm_achar_val(x) ((unsigned char) (escm_intptr) (x)->ptr)

void escm_achars_init(escm *);
size_t escm_achar_tget(void);
escm_atom *escm_achar_make(escm *, int);

escm_atom *escm_achar_p(escm *, escm_atom *, void *);

escm_atom *escm_achar_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_le_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_achar_ci_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_ci_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_ci_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_ci_le_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_ci_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_achar_alphabetic_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_numeric_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_whitespace_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_upper_case_p(escm *, escm_atom *, void *);
escm_atom *escm_achar_lower_case_p(escm *, escm_atom *, void *);

escm_atom *escm_achar_to_integer(escm *, escm_atom *, void *);
escm_atom *escm_integer_to_achar(escm *, escm_atom *, void *);

escm_atom *escm_achar_upcase(escm *, escm_atom *, void *);
escm_atom *escm_achar_downcase(escm *, escm_atom *, void *);

#endif /* ESCHEME_ACHARS_H */
