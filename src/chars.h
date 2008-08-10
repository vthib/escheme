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

#include "types.h"

#ifndef ESCM_USE_UNICODE

# include "achars.h"

# define ESCM_TYPE_CHAR ESCM_TYPE_ACHAR
# define ESCM_ISCHAR ESCM_ISACHAR
# define escm_char_val escm_achar_val
# define escm_char_make escm_achar_make

#else
# include "achars.h"
# include "uchars.h"

# define ESCM_TYPE_CHAR (ESCM_TYPE_ACHAR | ESCM_TYPE_UCHAR)

# define ESCM_ISCHAR(x) ((x)->type == ESCM_TYPE_CHAR)
# define escm_char_val(x)                                                \
    ((escm_type_ison(ESCM_TYPE_ACHAR)) ? escm_achar_val(x) : escm_uchar_val(x))
# define escm_char_make(e, c)                                                \
    ((escm_type_ison(ESCM_TYPE_ACHAR)) ? escm_achar_make(e, c) :        \
     escm_uchar_make(e, c))
#endif

#endif /* ESCHEME_CHARS_H */
