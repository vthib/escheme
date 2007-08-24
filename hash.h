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
#ifndef ESCHEME_HASH_H
# define ESCHEME_HASH_H

#include "types.h"

struct escm_hash {
    struct item **map; /**< the map containing all the items */

    unsigned long size; /**< the hash's size */

    unsigned sizelog2; /**< the hash's size in 2^n */
};

escm_hash *escm_hash_new(unsigned long);
void escm_hash_free(escm_hash *);

void escm_hash_set(escm_hash *, const char *, const void *);
void escm_hash_setfull(escm_hash *, const char *, const void *, Escm_Fun_Free);
void *escm_hash_get(escm_hash *, const char *);

void escm_hash_del(escm_hash *, const char *);

void escm_hash_foreach(escm_hash *, Escm_Fun_Foreach, void *);

/** @} */

#endif /* ESCHEME_HASH_H */
