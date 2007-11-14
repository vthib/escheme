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
#ifndef ESCHEME_NUMBERS_H
# define ESCHEME_NUMBERS_H

#include "types.h"

#ifndef ESCM_USE_COMPLETE_NUMBERS

# include "bnumbers.h"

# define ESCM_TYPE_NUMBER ESCM_TYPE_BNUMBER
# define ESCM_ISNUMBER ESCM_ISBNUMBER
# define ESCM_ISINT ESCM_ISBINT
# define ESCM_ISREAL ESCM_ISBREAL
# define escm_number_ival escm_bnumber_ival
# define escm_number_rval escm_bnumber_rval
# ifdef ESCM_INTBOOL
#  define ESCM_ISTRUE ESCM_ISBTRUE
# endif
# define escm_int_make escm_bint_make
# define escm_real_make escm_breal_make

# define ESCM_NUMBER_EXACTP ESCM_BNUMBER_EXACTP

#elif !defined ESCM_USE_BASIC_NUMBERS

# include "cnumbers.h"

# define ESCM_TYPE_NUMBER ESCM_TYPE_CNUMBER
# define ESCM_ISNUMBER ESCM_ISCNUMBER
# define ESCM_ISINT ESCM_ISCINT
# define ESCM_ISREAL ESCM_ISCREAL
# define escm_number_ival escm_cnumber_ival
# define escm_number_rval escm_cnumber_rval
# ifdef ESCM_INTBOOL
#  define ESCM_ISTRUE ESCM_ISCTRUE
# endif
# define escm_int_make escm_cint_make
# define escm_real_make escm_creal_make

# define ESCM_NUMBER_EXACTP ESCM_CNUMBER_EXACTP

#else
# include "bnumbers.h"
# include "cnumbers.h"

# define ESCM_TYPE_NUMBER (ESCM_TYPE_CNUMBER | ESCM_TYPE_BNUMBER)

# define ESCM_ISNUMBER(x) ((x)->type == ESCM_TYPE_NUMBER)
# define ESCM_ISINT(x)							\
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? ESCM_ISCINT(x) : ESCM_ISBINT(x))
# define ESCM_ISREAL(x)							\
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? ESCM_ISCREAL(x) : ESCM_ISBREAL(x))

# define escm_number_ival(x)						\
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? escm_cnumber_ival(x) :	\
     escm_bnumber_ival(x))
# define escm_number_rval(x)						\
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? escm_cnumber_rval(x) :	\
     escm_bnumber_rval(x))

# ifdef ESCM_INTBOOL
#  define ESCM_ISTRUE(x) \
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? ESCM_ISCTRUE(x) : ESCM_ISBTRUE(x))
# endif

# define escm_int_make(e, i) \
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? escm_cint_make(e, i) :	\
     escm_bint_make(e, i))
# define escm_real_make(e, i)						\
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? escm_creal_make(e, i) :	\
     escm_breal_make(e, i))

# define ESCM_NUMBER_EXACTP(x)						\
    ((escm_type_ison(ESCM_TYPE_CNUMBER)) ? ESCM_CNUMBER_EXACTP(x) :	\
     ESCM_BNUMBER_EXACTP(x))

#endif

#endif /* ESCHEME_NUMBERS_H */
