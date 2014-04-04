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
#include <stdlib.h>

#include "tst.h"
#include "utils.h"

static escm_tst *createpath(escm_tst **, const tchar *s);

escm_tst *
escm_tst_gettree(escm_tst **t, const tchar *s)
{
    escm_tst *tree;

    tree = createpath(t, s);
    if (!tree->symname)
        tree->symname = tcsdup(s);
    return tree;
}

escm_atom *
escm_tst_get(escm_tst *t, const tchar *s)
{
    if (!t)
        return NULL;

    if (*s < t->cval)
        return escm_tst_get(t->lo, s);
    else if (*s > t->cval)
        return escm_tst_get(t->hi, s);
    else {
        if (*(s + 1) == T('\0'))
            return t->node->atom;
        else
            return escm_tst_get(t->down, s + 1);
    }
}

void
escm_tst_add(escm_tst *t, escm_atom *atom)
{
    escm_tstnode *node;

    node = xmalloc(sizeof *node);
    node->atom = atom;
    node->prev = t->node, t->node = node;
}

void
escm_tst_foreach(escm_tst *t, void (*f)(escm *, escm_atom *), escm *e)
{
    struct escm_tstnode *node;

    if (!t)
        return;

    for (node = t->node; node; node = node->prev)
        f(e, node->atom);

    escm_tst_foreach(t->lo, f, e);
    escm_tst_foreach(t->down, f, e);
    escm_tst_foreach(t->hi, f, e);
}

void
escm_tst_free(escm_tst *t)
{
    escm_tstnode *node, *prev;

    if (!t)
        return;

    escm_tst_free(t->lo);
    escm_tst_free(t->down);
    escm_tst_free(t->hi);

    for (node = t->node; node; node = prev)
      prev = node->prev, free(node);
    free(t->symname);
    free(t);
}

static escm_tst *
createpath(escm_tst **t, const tchar *s)
{
    if (*s == T('\0'))
        return NULL;

    if (!*t) {
        *t = xcalloc(1, sizeof **t);
        (*t)->cval = *s;
    }

    if (*s < (*t)->cval)
        return createpath(&(*t)->lo, s);
    else if (*s > (*t)->cval)
        return createpath(&(*t)->hi, s);
    else {
        if (*(s + 1) == T('\0'))
            return *t;
        else
            return createpath(&(*t)->down, s + 1);
    }
}
