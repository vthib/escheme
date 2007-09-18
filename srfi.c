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
#include "escheme.h"

void
escm_srfi_init(escm *e)
{
    /* srfi 6 */
    escm_procedure_new(e, "open-input-string", 1, 1,
		       escm_srfi_open_input_string, NULL);

    /* srfi 23 */
    escm_procedure_new(e, "error", 1, -1, escm_srfi_error, NULL);
}

/* srfi 6 */
escm_atom *
escm_srfi_open_input_string(escm *e, escm_atom *args)
{
    escm_atom *str;
    escm_port *p;

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    p = xcalloc(1, sizeof *p);
    p->input = 1;
    p->d.input = escm_input_str(escm_str_val(str));

    return escm_atom_new(e, ESCM_TYPE_PORT, p);
}

/* srfi 23 */
escm_atom *
escm_srfi_error(escm *e, escm_atom *args)
{
    escm_atom *reason;

    reason = escm_cons_pop(e, &args);

    fprintf(stderr, "Error: ");
    escm_atom_print0(e, reason, stderr, 1);
    if (args)
	fprintf(stderr, ": ");
    while (args) {
	reason = escm_cons_pop(e, &args);
	escm_atom_print(e, reason, stderr);
	if (args)
	    fprintf(stderr, ": ");
    }
    fprintf(stderr, "\n");

    e->err = -1;
    return NULL;
}
