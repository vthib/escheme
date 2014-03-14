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
        ftprintf(stderr, _(T("Memory is too low\n")));
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
        ftprintf(stderr, _(T("Memory is too low\n")));
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
        ftprintf(stderr, _(T("Memory is too low\n")));
        exit(EXIT_FAILURE);
    }

    return p;
}

/**
 * @brief duplicate a string and return it
 */
tchar *
tcsdup(const tchar *s)
{
    size_t len;
    tchar *copy;

    len = tcslen(s) + 1;
    copy = xmalloc(len * sizeof *copy);
    tmemcpy(copy, s, len);

    return copy;
}

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
xtcscasecmp(const tchar *s1, const tchar *s2)
{
    tchar *c1, *c2;

    for (c1 = (tchar *) s1, c2 = (tchar *) s2; *c1 != T('\0'); c1++, c2++) {
        if (*c2 == T('\0'))
            return 1;
        else if (totlower(*c1) != totlower(*c2))
            return (totlower(*c1) > totlower(*c2)) ? 1 : -1;
    }

    return (*c2 != T('\0')) ? -1 : 0;
}

double
xround(double a)
{
    double c;

    c = ceil(a);

    if (DBL_EQ(c - a, 0.5)) { /* round to even */
		if (((long) c) % 2 == 0)
            return c;
        else
            return c - 1;
    } else if (DBL_LT(c, 0.5)) /* a is closer to his upper integer */
        return c;
    else
        return c - 1;
}

char *
tcstostr(const tchar *w)
{
#ifdef ESCM_UNICODE
    char *str;
    size_t n;

    n = wcstombs(NULL, w, 0) + 1;
    str = xmalloc(sizeof *str * n);
    if (wcstombs(str, w, n) == (size_t) (-1))
        ftprintf(stderr, _(T("wcstombs: conversion error.\n")));
    return str;
#else
    return xstrdup(w);
#endif
}

tchar *
strtotcs(const char *str)
{
#ifdef ESCM_UNICODE
    wchar_t *w;
    size_t n;

    n = mbstowcs(NULL, str, 0) + 1;
    w = xmalloc(sizeof *w * n);
    if (mbstowcs(w, str, n) == (size_t) (-1))
        ftprintf(stderr, _(T("mbstowcs: conversion error.\n")));
    return w;
#else
    return xstrdup(str);
#endif

}

/**@}*/
