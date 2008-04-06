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
#ifndef ESCHEME_TST_H
# define ESCHEME_TST_H

#include "types.h"

struct escm_tstnode {
    escm_atom *atom;
    escm_tstnode *prev;
};

struct escm_tst {
    escm_tst *lo, *hi, *down;
    char cval;
    escm_tstnode *node;
    char *symname;
};

escm_tst *escm_tst_gettree(escm_tst **, const char *);
escm_atom *escm_tst_get(escm_tst *, const char *);
void escm_tst_add(escm_tst *, escm_atom *);

void escm_tst_free(escm_tst *);

void escm_tst_foreach(escm_tst *, void (*)(escm *, escm_atom *), escm *);

#endif /* ESCHEME_TST_H */
