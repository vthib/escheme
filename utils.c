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

#include "utils.h"

/**
 * @brief alloc @p n bytes
 */
/*@out@*/ /*@only@*/ void *
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
/*@only@*/ void *
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
/*@only@*/ void *
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
/*@only@*/ char *
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
	else if (*c1 != *c2)
	    return (*c1 > *c2) ? 1 : -1;
    }

    return (*c2 != '\0') ? -1 : 0;
}

/**@}*/
