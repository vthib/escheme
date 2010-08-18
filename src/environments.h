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
#ifndef ESCHEME_ENVIRONMENTS_H
# define ESCHEME_ENVIRONMENTS_H

#include "types.h"

typedef struct escm_env {
    struct envlist *list;
    escm_atom *prev;
} escm_env;

#define ESCM_TYPE_ENV (escm_env_tget())
#define ESCM_ISENV(x) ((x)->type == ESCM_TYPE_ENV)
#define escm_env_val(x) ((escm_env *) (x)->ptr)

void escm_environments_init(escm *);
void escm_env_addprimitives(escm *);
size_t escm_env_tget(void);

escm_atom *escm_env_new(escm *, escm_atom *);

void escm_env_set(escm *, escm_atom *, escm_atom *, escm_atom *);

escm_atom *escm_env_enter(escm *, escm_atom *);
void escm_env_leave(escm *, escm_atom *);

escm_atom *escm_prim_eval(escm *, escm_atom *, void *);

escm_atom *escm_library(escm *, escm_atom *, void *);
escm_atom *escm_import(escm *, escm_atom *, void *);

escm_atom *escm_library_enter(escm *, tchar *, int);
void escm_library_export(escm *, escm_atom *, tchar *);
void escm_library_exit(escm *);

escm_atom *escm_alpha(escm *, escm_atom *, void *);
escm_atom *escm_with(escm *, escm_atom *, void *);
escm_atom *escm_scheme_report_environment(escm *, escm_atom *, void *);
escm_atom *escm_interaction_environment(escm *, escm_atom *, void *);

#endif /* ESCHEME_ENVIRONMENTS_H */
