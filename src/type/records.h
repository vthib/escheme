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
#ifndef ESCHEME_RECORDS_H
# define ESCHEME_RECORDS_H

#ifdef ESCM_USE_RECORDS

#include "types.h"

#define escm_rec_val(rec) ((escm_atom **) (rec)->ptr)

void escm_records_init(escm *);

escm_atom *escm_define_record(escm *, escm_atom *, void *);

#endif /* ESCM_USE_RECORDS */

#endif /* ESCHEME_RECORDS_H */
