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

#include "types.h"

#ifndef ESCM_USE_UNICODE

# include "astrings.h"

# define ESCM_TYPE_STRING ESCM_TYPE_ASTRING
# define ESCM_ISSTR ESCM_ISASTR
# define escm_str_val escm_astr_val
# define escm_str_len escm_astr_len
# define escm_string_make escm_astring_make
# define escm_string_make2(e, str) escm_astring_make(e, str, strlen(str))

#else
# include "astrings.h"
# include "ustrings.h"

# define ESCM_TYPE_STRING (ESCM_TYPE_ASTRING | ESCM_TYPE_USTRING)

# define ESCM_ISSTR(x) ((x)->type == ESCM_TYPE_STRING)
# define escm_str_val(x)                                                \
    ((escm_type_ison(ESCM_TYPE_ASTRING)) ? escm_astr_val(x) : escm_ustr_val(x))
# define escm_str_len(x)                                                \
    ((escm_type_ison(ESCM_TYPE_ASTRING)) ? escm_astr_len(x) : escm_ustr_len(x))
# define escm_string_make(e, str, len)                                  \
    ((escm_type_ison(ESCM_TYPE_ASTRING)) ? escm_astring_make(e, str, len) \
     : escm_ustring_make(e, str, len))
# define escm_string_make2(e, str)                                      \
    ((escm_type_ison(ESCM_TYPE_ASTRING)) ?                              \
     escm_astring_make(e, str, strlen(str)) : escm_ustring_make2(e, str))

#endif

#endif /* ESCHEME_STRINGS_H */
