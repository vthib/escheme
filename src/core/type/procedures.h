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
#ifndef ESCHEME_PROCEDURES_H
# define ESCHEME_PROCEDURES_H

#include "types.h"

#define ESCM_TYPE_PROC (escm_proc_tget())

#define ESCM_ISPROC(x) ((x)->type == ESCM_TYPE_PROC)
#define ESCM_ISCLOSURE(x) (ESCM_ISPROC(x) && \
                           (escm_proc_val(x)->type == ESCM_CLOSURE))

#define escm_proc_val(x) ((escm_procedure *) (x)->ptr)

typedef struct escm_procedure {
    tchar *name;

    union {
        struct {
            unsigned int min;
            int max;
            unsigned int quoted;

            void *data;

            Escm_Fun_Prim fun;
        } c;
        struct {
            escm_atom *code;
            escm_atom *env;

            escm_atom *args;
        } closure;
    } d;

    enum { ESCM_PRIMITIVE, ESCM_CLOSURE } type;
} escm_procedure;

void escm_procedures_init(escm *);
size_t escm_proc_tget(void);

escm_atom *escm_procedure_new(escm *, const tchar *, unsigned int, int,
                              Escm_Fun_Prim, void *);

escm_atom *escm_procedure_exec(escm *, escm_atom *, escm_atom *, int);

#endif /* ESCHEME_PROCEDURES_H */
