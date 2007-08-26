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
#ifndef ESCHEME_TYPES_H
# define ESCHEME_TYPES_H

#include <stdio.h>

#ifdef ESCM_R5RS
# define ESCM_USE_BOOLEANS
# define ESCM_USE_CHARACTERS
# define ESCM_USE_VECTORS
# define ESCM_USE_STRINGS
# define ESCM_USE_NUMBERS

# define ESCM_CIRCULAR_LIST 1 /* 0: no handle, 1: just in list? and length
				 2: check also when printing */
#endif

#ifndef ESCM_CIRCULAR_LIST
# define ESCM_CIRCULAR_LIST 0 /* default */
#endif

#if defined ESCM_INTBOOL && defined ESCM_USE_BOOLEANS
# undef ESCM_INTBOOL /* use boolean instead of integers */
#endif

#if !defined ESCM_USE_BOOLEANS && !defined ESCM_INTBOOL
# error "At least one of the boolean and number types need to be activated."
#endif


/* typedefs */

#if __WORDSIZE == 64
typedef long escm_intptr;
#else
typedef int escm_intptr;
#endif

typedef struct escm escm;
typedef struct escm_context escm_context;
typedef struct escm_atom escm_atom;
typedef struct escm_hash escm_hash;
typedef struct escm_input escm_input;
typedef struct escm_type escm_type;

typedef void (*Escm_Fun_Mark)(escm *, void *);
typedef void (*Escm_Fun_Free)(void *);
typedef void (*Escm_Fun_Print)(escm *, void *, FILE *);
typedef int (*Escm_Fun_Equal)(escm *, void *, void *, unsigned int);

typedef int (*Escm_Fun_Parsetest)(escm *, int);
typedef escm_atom *(*Escm_Fun_Parse)(escm *);
typedef escm_atom *(*Escm_Fun_Eval)(escm *, void *);

typedef void (*Escm_Fun_Foreach)(void *, void *);
typedef int (*Escm_Fun_Match)(void *, void *);
typedef escm_atom *(*Escm_Fun_Prim)(escm *, escm_atom *);

#endif /* ESCHEME_TYPES_H */
