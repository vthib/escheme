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
# define ESCM_USE_NUMBERS
# define ESCM_USE_BOOLEANS
# define ESCM_USE_CHARACTERS
# define ESCM_USE_VECTORS
# define ESCM_USE_STRINGS
# define ESCM_USE_PROMISES
# define ESCM_USE_MACROS
# define ESCM_USE_PORTS
# define ESCM_USE_MATH

/* Disabled it until it really works (not for tomorrow though)
# define ESCM_USE_CONTINUATIONS
*/

# define ESCM_CIRCULAR_LIST 2 /* 0: no handle, 1: just in list? and length
                                 2: check also when printing */
#endif

#if defined ESCM_UNICODE && !defined ESCM_USE_C99
# error "unicode needs c99"
#endif

#if defined ESCM_USE_DYNTYPES && !defined ESCM_USE_NUMBERS
# error "dynamic types needs number type"
#endif

#ifndef ESCM_CIRCULAR_LIST
# define ESCM_CIRCULAR_LIST 0 /* default */
#endif

/* typedefs */

#ifdef ESCM_UNICODE

# include <wchar.h>
# include <wctype.h>

typedef wchar_t tchar;
typedef wint_t tint;

# define TEOF WEOF
# define T(a) L ## a
# define TFMT L"l"

# define tcslen wcslen
# define tcscmp wcscmp
# define tcschr wcschr

# define tmemcpy wmemcpy
# define tmemset wmemset

# define totlower towlower
# define totupper towupper
# define istalnum iswalnum
# define istalpha iswalpha
# define istdigit iswdigit
# define istlower iswlower
# define istprint iswprint
# define istspace iswspace
# define istupper iswupper
# define istxdigit iswxdigit

# define fgettc fgetwc
# define fputtc fputwc

# define tprintf wprintf
# define ftprintf fwprintf
# define vftprintf vfwprintf
# define vsntprintf vswprintf
# define sntprintf swprintf

# define tcstod wcstod
# define tcstol wcstol

#else

typedef char tchar;
typedef int tint;

# define TEOF EOF
# define T(a) a
# define TFMT

# define tcslen strlen
# define tcscmp strcmp
# define tcschr strchr

# define tmemcpy memcpy
# define tmemset memset

# define totlower tolower
# define totupper toupper
# define istalnum isalnum
# define istalpha isalpha
# define istdigit isdigit
# define istlower islower
# define istprint isprint
# define istspace isspace
# define istupper isupper
# define istxdigit isxdigit

# define fgettc fgetc
# define fputtc fputc

# define tprintf printf
# define ftprintf fprintf
# define vftprintf vfprintf
# define vsntprintf vsnprintf
# define sntprintf snprintf

# define tcstod strtod
# define tcstol strtol

#endif

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
typedef struct escm_output escm_output;
typedef struct escm_type escm_type;
typedef struct escm_tst escm_tst;
typedef struct escm_tstnode escm_tstnode;

typedef void (*Escm_Fun_Mark)(escm *, void *);
typedef void (*Escm_Fun_Free)(void *);
typedef void (*Escm_Fun_Print)(escm *, void *, escm_output *, int);
typedef int (*Escm_Fun_Equal)(escm *, void *, void *, int);

typedef int (*Escm_Fun_Parsetest)(escm *, escm_input *, tint);
typedef escm_atom *(*Escm_Fun_Parse)(escm *, escm_input *);
typedef escm_atom *(*Escm_Fun_Eval)(escm *, void *);
typedef escm_atom *(*Escm_Fun_Exec)(escm *, void *, escm_atom *);

typedef void (*Escm_Fun_Exit)(escm *, void *);

typedef void (*Escm_Fun_Foreach)(void *, void *);
typedef int (*Escm_Fun_Match)(void *, void *);
typedef escm_atom *(*Escm_Fun_Prim)(escm *, escm_atom *, void *);

#endif /* ESCHEME_TYPES_H */
