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
#ifndef ESCHEME_PROMISES_H
# define ESCHEME_PROMISES_H

#include "types.h"

#define ESCM_TYPE_PROMISE escm_promise_tget()

#define ESCM_ISPROMISE(x) ((x)->type == ESCM_TYPE_PROMISE)

typedef struct escm_promise {
    escm_atom *atom;
    escm_atom *env;
} escm_promise;

void escm_promises_init(escm *);
size_t escm_promise_tget(void);

escm_atom *escm_delay(escm *, escm_atom *);
escm_atom *escm_force(escm *, escm_atom *);

#endif /* ESCHEME_PROMISES_H */
