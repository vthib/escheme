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
#ifndef ESCHEME_SCMPF_H
# define ESCHEME_SCMPF_H

# include "types.h"

typedef escm_atom *(*Escm_Fun_Next_Args)(void *);

void escm_scmpf(escm *, escm_output *, const tchar *, ...);

/* A trick to make scmpf work with variadic macros (see escm_notice, ...) */
void escm_scmpf_2(escm *, escm_output *, ...);

void escm_scmpf_fun(escm *e, escm_output *stream, const tchar *format,
					void *data, Escm_Fun_Next_Args next);

#endif /* ESCHEME_SCMPF_H */
