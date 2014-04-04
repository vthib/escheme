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
#ifndef ESCHEME_CHARS_H
# define ESCHEME_CHARS_H

#ifdef ESCM_USE_CHARACTERS

# include "types.h"

# define ESCM_TYPE_CHAR escm_char_tget()

# define ESCM_ISCHAR(x) ((x)->type == ESCM_TYPE_CHAR)

# define escm_char_val(x) ((tint) (escm_intptr) (x)->ptr)

void escm_chars_init(escm *);
size_t escm_char_tget(void);
escm_atom *escm_char_make(escm *, tint);

escm_atom *escm_char_p(escm *, escm_atom *, void *);

escm_atom *escm_char_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_char_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_char_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_char_le_p(escm *, escm_atom *, void *);
escm_atom *escm_char_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_char_ci_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_char_ci_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_char_ci_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_char_ci_le_p(escm *, escm_atom *, void *);
escm_atom *escm_char_ci_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_char_alphabetic_p(escm *, escm_atom *, void *);
escm_atom *escm_char_numeric_p(escm *, escm_atom *, void *);
escm_atom *escm_char_whitespace_p(escm *, escm_atom *, void *);
escm_atom *escm_char_upper_case_p(escm *, escm_atom *, void *);
escm_atom *escm_char_lower_case_p(escm *, escm_atom *, void *);

escm_atom *escm_char_to_integer(escm *, escm_atom *, void *);
escm_atom *escm_integer_to_char(escm *, escm_atom *, void *);

escm_atom *escm_char_upcase(escm *, escm_atom *, void *);
escm_atom *escm_char_downcase(escm *, escm_atom *, void *);

#endif /* ESCM_USE_CHARACTERS */
#endif /* ESCHEME_CHARS_H */
