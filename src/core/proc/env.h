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
#ifndef ESCHEME_PROC_ENV_H
# define ESCHEME_PROC_ENV_H

#include "types.h"

void escm_addprims_env(escm *);

escm_atom *escm_prim_eval(escm *, escm_atom *, void *);
escm_atom *escm_prim_begin(escm *, escm_atom *, void *);

escm_atom *escm_library(escm *, escm_atom *, void *);
escm_atom *escm_import(escm *, escm_atom *, void *);

escm_atom *escm_alpha(escm *, escm_atom *, void *);
escm_atom *escm_with(escm *, escm_atom *, void *);
escm_atom *escm_scheme_report_environment(escm *, escm_atom *, void *);
escm_atom *escm_interaction_environment(escm *, escm_atom *, void *);

escm_atom *escm_begin(escm *, escm_atom *, int);

#endif /* ESCHEME_PROC_ENV_H */
