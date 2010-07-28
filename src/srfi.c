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

#include "escheme.h"

void
escm_srfi_init(escm *e)
{
#if defined ESCM_USE_STRINGS && defined ESCM_USE_PORTS
/*    (void) escm_library_enter(e, "srfi-6", 1);*/

    (void) escm_procedure_new(e, "open-input-string", 1, 1,
                              escm_open_input_string, NULL);
    (void) escm_procedure_new(e, "open-output-string", 0, 0,
                              escm_open_output_string, NULL);
    (void) escm_procedure_new(e, "get-output-string", 1, 1,
                              escm_get_output_string, NULL);

/*    escm_library_exit(e); */
#endif
/*    (void) escm_library_enter(e, "srfi-23", 1); */

    (void) escm_procedure_new(e, "error", 1, -1, escm_srfi_error, NULL);

/*    escm_library_exit(e); */
}

#if defined ESCM_USE_STRINGS && defined ESCM_USE_PORTS
/* srfi 6 */
escm_atom *
escm_open_input_string(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_STRING)) {
        escm_error(e, "~s: string type is off.~%", escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
        escm_error(e, "~s: port type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

#ifdef ESCM_USE_UNICODE
    if (escm_type_ison(ESCM_TYPE_USTRING)) {
        char *s;
        escm_atom *atom;

        s = wcstostr(escm_ustr_val(str));
        atom = escm_port_make(e, escm_input_str(s), 1);
        free(s);
        return atom;
    } else
#endif
     return escm_port_make(e, escm_input_str(escm_astr_val(str)), 1);
}

escm_atom *
escm_open_output_string(escm *e, escm_atom *args, void *nil)
{
    (void) args;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_STRING)) {
        escm_error(e, "~s: string type is off.~%", escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
        escm_error(e, "~s: port type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    return escm_port_make(e, escm_output_str(), 0);
}

escm_atom *
escm_get_output_string(escm *e, escm_atom *args, void *nil)
{
    escm_atom *port;
    escm_output *outp;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_STRING)) {
        escm_error(e, "~s: string type is off.~%", escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
        escm_error(e, "~s: port type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    port = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(port), port, e);

    if (escm_port_val(port)->input) {
        escm_error(e, "~s: given port is not an output port.~%", escm_fun(e));
        escm_abort(e);
    }

    outp = escm_port_val(port)->d.output;

# ifdef ESCM_USE_UNICODE
    if (escm_type_ison(ESCM_TYPE_USTRING))
        return escm_ustring_make2(e, escm_output_getstr(outp));
    else
# endif
    return escm_astring_make(e, escm_output_getstr(outp),
                             outp->d.str.cur - outp->d.str.str);
}
#endif

/* srfi 23 */
escm_atom *
escm_srfi_error(escm *e, escm_atom *args, void *nil)
{
    escm_atom *reason;

    (void) nil;
    reason = escm_cons_pop(e, &args);

    escm_printf(e->errp, "error: ");
    escm_atom_print4(e, reason, e->errp, 1);
    if (args != e->NIL)
        escm_printf(e->errp, ": ");
    while (args != e->NIL) {
        reason = escm_cons_pop(e, &args);
        escm_atom_print4(e, reason, e->errp, 1);
        escm_putc(e->errp, ' ');
    }
    escm_putc(e->errp, '\n');

    escm_abort(e);
}
