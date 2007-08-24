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
#ifndef ESCHEME_ENVIRONMENT_H
# define ESCHEME_ENVIRONMENT_H

#include "types.h"

typedef struct escm_env {
    union {
	escm_hash *toplvl;
	struct {
	    struct escm_node *first;
	    struct escm_node *last;
	} lst;
    } d;

    escm_atom *prev;
} escm_env;

void escm_env_init(escm *);

escm_atom *escm_env_new(escm *, escm_atom *);

escm_atom *escm_env_get(escm_atom *, const char *);
void escm_env_set(escm_atom *, const char *, escm_atom *);

escm_atom *escm_env_enter(escm *, escm_atom *);
void escm_env_leave(escm *, escm_atom *);

#endif /* ESCHEME_ENVIRONMENT_H */
