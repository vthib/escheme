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
#if defined ESCM_USE_STRINGS && defined ESCM_USE_PORTS
    /* srfi 6 */
    (void) escm_procedure_new(e, "open-input-string", 1, 1,
			      escm_open_input_string, NULL);
    (void) escm_procedure_new(e, "open-output-string", 0, 0,
			      escm_open_output_string, NULL);
    (void) escm_procedure_new(e, "get-output-string", 1, 1,
			      escm_get_output_string, NULL);
#endif

    /* srfi 23 */
    (void) escm_procedure_new(e, "error", 1, -1, escm_srfi_error, NULL);
}

#if defined ESCM_USE_STRINGS && defined ESCM_USE_PORTS
/* srfi 6 */
escm_atom *
escm_open_input_string(escm *e, escm_atom *args)
{
    escm_atom *str;

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
	escm_error(e, "~s: port type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    return escm_port_make(e, escm_input_str(escm_str_val(str)), 1);
}

escm_atom *
escm_open_output_string(escm *e, escm_atom *args)
{
    (void) args;

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
	escm_error(e, "~s: port type is off.~%", e->curobj);
	escm_abort(e);
    }

    return escm_port_make(e, escm_output_str(), 0);
}

escm_atom *
escm_get_output_string(escm *e, escm_atom *args)
{
    escm_atom *port;
    escm_output *outp;

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
	escm_error(e, "~s: port type is off.~%", e->curobj);
	escm_abort(e);
    }

    port = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(port), port, e);

    if (escm_port_val(port)->input) {
	fprintf(stderr, "get-output-string: given port is not an output "
		"port.\n");
	escm_abort(e);
    }

    outp = escm_port_val(port)->d.output;
    return escm_string_make(e, escm_output_getstr(outp),
			    outp->d.str.cur - outp->d.str.str);
}
#endif

/* srfi 23 */
escm_atom *
escm_srfi_error(escm *e, escm_atom *args)
{
    escm_atom *reason;

    reason = escm_cons_pop(e, &args);

    fprintf(stderr, "Error: ");
    escm_atom_print4(e, reason, e->errp, 1);
    if (args)
	fprintf(stderr, ": ");
    while (args) {
	reason = escm_cons_pop(e, &args);
	escm_atom_print4(e, reason, e->errp, 1);
    }
    fprintf(stderr, "\n");

    escm_abort(e);
}
