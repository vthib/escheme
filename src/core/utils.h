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
#ifndef ESCHEME_UTILS_H
# define ESCHEME_UTILS_H

#include <stdlib.h>
#include <float.h>
#include "types.h"

#ifdef _
# undef _
#endif
#ifdef HAVE_LIBINTL_H
# include <libintl.h>
# define _(str) gettext(str)
#else
# define _(str) str
#endif

#if !defined(DBL_EPSILON) && !defined(S_SPLINT_S)
# define DBL_EPSILON 1E-9
#endif

#define ABS(x) (((x) < 0) ? (-(x)) : (x))
#define DABS(x) ((DBL_LT(x, 0.)) ? (-(x)) : (x))

#define DBL_EQ(a, b) (fabs((double) (a) - (b)) < DBL_EPSILON)
#define DBL_GT(a, b) (((double) (a) - (b)) >= DBL_EPSILON)
#define DBL_LT(a, b) DBL_GT(b, a)
#define DBL_GE(a, b) (((double) (a) - (b)) >= 0)
#define DBL_LE(a, b) DBL_GE(b, a)

void *xmalloc(size_t);
void *xcalloc(size_t, size_t);
void *xrealloc(void *, size_t);
tchar *tcsdup(const tchar *);
char *xstrdup(const char *);

int xtcscasecmp(const tchar *, const tchar *);
double xround(double);

char *tcstostr(const tchar *);
tchar *strtotcs(const char *);

#define xfree(p) free(p), (p) = NULL;

#endif /* ESCHEME_UTILS_H */
