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
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "types.h"

#ifdef ESCM_USE_UNICODE
#include <wchar.h>
#endif

#include "utils.h"

/**
 * @brief alloc @p n bytes
 */
void *
xmalloc(size_t n)
{
    void *p;

    p = malloc(n);
    if (!p) {
        fprintf(stderr, _("Memory is too low\n"));
	exit(EXIT_FAILURE);
    }

    return p;
}

/**
 * @brief alloc @p nelem elements of @p n bytes  and set the memory to zero
 */
void *
xcalloc(size_t nelem, size_t n)
{
    void *p;

    p = calloc(nelem, n);
    if (!p) {
        fprintf(stderr, _("Memory is too low\n"));
        exit(EXIT_FAILURE);
    }
    return p;
}

/**
 * @brief reallocate a pointer
 */
void *
xrealloc(void *ptr, size_t size)
{
    void *p;

    p = realloc(ptr, size);
    if (!p) {
        fprintf(stderr, _("Memory is too low\n"));
        exit(EXIT_FAILURE);
    }

    return p;    
}

/**
 * @brief duplicate a string and return it
 */
char *
xstrdup(const char *s)
{
    size_t len;
    char *copy;

    len = strlen(s) + 1;
    copy = xmalloc(sizeof(char) * len);
    memcpy(copy, s, len);

    return copy;
}

int
xstrcasecmp(const char *s1, const char *s2)
{
    char *c1, *c2;

    for (c1 = (char *) s1, c2 = (char *) s2; *c1 != '\0'; c1++, c2++) {
	if (*c2 == '\0')
	    return 1;
	else if (tolower(*c1) < tolower(*c2))
	    return -1;
	else if (tolower(*c1) > tolower(*c2))
		return 1;
    }

    return (*c2 != '\0') ? -1 : 0;
}

double
xround(double a)
{
#ifdef ESCM_USE_UNICODE
    return rint(a);
#else
    double c;

    c = ceil(a);

    if (DBL_EQ(a - c, 0.5)) { /* round to even */
	if (((long) c) % 2 == 0)
	    return c;
	else
	    return c - 1;
    } else if (DBL_LT(c, 0.5)) /* a is closer to his upper integer */
	return c;
    else
	return c - 1;
#endif
}

char *
wcstostr(const wchar_t *w)
{
    char *str;
    size_t n;

    n = wcstombs(NULL, w, 0) + 1;
    str = xmalloc(sizeof *str * n);
    if (wcstombs(str, w, n) == (size_t) (-1))
	fprintf(stderr, "wcstombs: conversion error.\n");
    return str;
}

wchar_t *
strtowcs(const char *str)
{
    wchar_t *w;
    size_t n;

    n = mbstowcs(NULL, str, 0) + 1;
    w = xmalloc(sizeof *w * n);
    if (mbstowcs(w, str, n) == (size_t) (-1))
	fprintf(stderr, "mbstowcs: conversion error.\n");
    return w;
}

#ifdef ESCM_USE_UNICODE
/**
 * @brief duplicate a wide string and return it
 */
wchar_t *
xwcsdup(const wchar_t *s)
{
    size_t len;
    wchar_t *copy;

    len = sizeof *s * (wcslen(s) + 1);
    copy = xmalloc(len);
    memcpy(copy, s, len);

    return copy;
}

int
xwcscasecmp(const wchar_t *s1, const wchar_t *s2)
{
    size_t i1, i2;

    for (i1 = 0, i2 = 0; s1[i1] != L'\0'; i1++, i2++) {
	if (s2[i2] == L'\0')
	    return 1;
	else if (s1[i1] != s2[i2])
	    return (s1[i1] > s2[i2]) ? 1 : -1;
    }

    return (s2[i2] != L'\0') ? -1 : 0;
}
#endif

/**@}*/
