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
#ifndef ESCHEME_CONTINUATIONS_H
# define ESCHEME_CONTINUATIONS_H

#include "types.h"
#include <setjmp.h>

/**
 * IMPORTANT
 *
 * This implementation of continuations is very minimalistic, not very
 * respectful with r5rs and very, very bugged.
 * It works only when using the continuation inside the "call/cc" call. All
 * attempts to save the continuation to call it later will make the program
 * crash.
 * I strongly recommend to disabled this type. Use it if on only if you know
 * what your doing, and if you're ready to die.
 *
 * The continuations don't work because it is impossible to do it in a portable
 * way. I only provide this as is. If you really need more complex
 * continuations, then go rethink your code or find a good scheme
 * interpreter/compiler.
 */

#define ESCM_TYPE_CONTINUATION escm_continuation_tget()

#define ESCM_ISCONTINUATION(x) ((x)->type == ESCM_TYPE_CONTINUATION)

#define escm_continuation_val(x) ((escm_continuation *) (x)->ptr)

typedef struct escm_continuation {
    jmp_buf buf;
    escm_atom *ret;
    escm_context *ctx;
} escm_continuation;

void escm_continuations_init(escm *);
size_t escm_continuation_tget(void);

void escm_continuation_exec(escm *, escm_atom *, escm_atom *);

escm_atom *escm_continuation_p(escm *, escm_atom *, void *);
escm_atom *escm_call_with_cc(escm *, escm_atom *, void *);
escm_atom *escm_call_with_values(escm *, escm_atom *, void *);
escm_atom *escm_values(escm *, escm_atom *, void *);

#endif /* ESCHEME_CONTINUATIONS_H */
