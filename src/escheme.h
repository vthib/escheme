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
#ifndef ESCHEME_ESCHEME_H
# define ESCHEME_ESCHEME_H

#include "types.h"
#include "base.h"
#include "primitives.h"
#include "srfi.h"

#ifdef ESCM_USE_DYNTYPES
# include "type/dyntypes.h"
#endif

#ifdef ESCM_USE_BOOLEANS
# include "type/booleans.h"
#endif

#ifdef ESCM_USE_NUMBERS
# include "type/numbers.h"
#endif

#ifdef ESCM_USE_CHARACTERS
# include "type/chars.h"
#endif

#ifdef ESCM_USE_STRINGS
# include "type/strings.h"
#endif

#ifdef ESCM_USE_PROMISES
# include "type/promises.h"
#endif

#ifdef ESCM_USE_VECTORS
# include "type/vectors.h"
#endif

#ifdef ESCM_USE_MACROS
# include "type/macros.h"
#endif

#ifdef ESCM_USE_PORTS
# include "type/ports.h"
#endif

#ifdef ESCM_USE_CONTINUATIONS
# include "type/continuations.h"
#endif

#ifdef ESCM_USE_RECORDS
# include "type/records.h"
#endif

#endif /* ESCHEME_ESCHEME_H */
