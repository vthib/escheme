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
#include "utils.h"
#include "input.h"
#include "output.h"
#include "escm.h"
#include "atom.h"
#include "tst.h"
#include "environments.h"
#include "cons.h"
#include "procedures.h"
#include "primitives.h"
#include "symbols.h"
#include "srfi.h"

#ifdef ESCM_USE_DYNTYPES
# include "dyntypes.h"
#endif

#ifdef ESCM_USE_BOOLEANS
# include "booleans.h"
#endif

#ifdef ESCM_USE_NUMBERS
# include "numbers.h"
#endif

#ifdef ESCM_USE_CHARACTERS
# include "chars.h"
#endif

#ifdef ESCM_USE_STRINGS
# include "strings.h"
#endif

#ifdef ESCM_USE_PROMISES
# include "promises.h"
#endif

#ifdef ESCM_USE_VECTORS
# include "vectors.h"
#endif

#ifdef ESCM_USE_MACROS
# include "macros.h"
#endif

#ifdef ESCM_USE_PORTS
# include "ports.h"
#endif

#ifdef ESCM_USE_CONTINUATIONS
# include "continuations.h"
#endif

#ifdef ESCM_USE_RECORDS
# include "records.h"
#endif

#endif /* ESCHEME_ESCHEME_H */
