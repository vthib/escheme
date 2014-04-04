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
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include "scmpf.h"
#include "output.h"
#include "atom.h"

static inline escm_atom *next_args(va_list *);

void
escm_scmpf(escm *e, escm_output *stream, const tchar *format, ...)
{
    va_list va;

    va_start(va, format);

    escm_scmpf_fun(e, stream, format, &va, (Escm_Fun_Next_Args) next_args);

    va_end(va);
}

void
escm_scmpf_2(escm *e, escm_output *stream, ...)
{
    va_list va;
	const tchar *format;

    va_start(va, stream);

    format = va_arg(va, const tchar *);
	escm_scmpf_fun(e, stream, format, &va, (Escm_Fun_Next_Args) next_args);

    va_end(va);
}

void
escm_scmpf_fun(escm *e, escm_output *stream, const tchar *format, void *data,
			   Escm_Fun_Next_Args next)
{
	const tchar *p;

	if (!stream) /* can pass a NULL stream to prevent any print:
					used by test_error for example */
		return;

	for (p = format; *p != T('\0'); p++) {
		if (*p == T('~')) {
			p++;
			switch (*p) {
			case T('~'):
				escm_putc(stream, T('~'));
				break;
			case T('a'):
				escm_atom_print4(e, next(data), stream, 1);
				break;
			case T('s'):
				escm_atom_print4(e, next(data), stream, 0);
				break;
			case T('n'): case T('%'):
				escm_putc(stream, T('\n'));
				break;
			default:
				escm_putc(stream, T('~'));
				escm_putc(stream, *p);
				break;
			}
		} else
			escm_putc(stream, *p);
	}
}

/* XXX: Non standard, as when we pass a va_list as a parameter, we should
 * use va_copy before using it, but then we would always have the first
 * argument over and over.
 * Solution 1: keep a counter and skip previous arguments. Not great.
 * Solution 2: Put all arguments in a dynamic array in the function with
 * variable arguments, then this function would just be "*p++". Better.
 */
static inline escm_atom *
next_args(va_list *v)
{
	return va_arg(*v, escm_atom *);
}
