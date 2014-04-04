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

#include "srfi.h"
#include "base.h"
#include "type/strings.h"
#include "type/ports.h"
#include "primitives.h"

void
escm_srfi_init(escm *e)
{
    escm_atom *o;

    o = escm_procedure_new(e, T("and-let*"), 1, -1, escm_and_let_star, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;

#if defined ESCM_USE_STRINGS && defined ESCM_USE_PORTS
/*    (void) escm_library_enter(e, "srfi-6", 1);*/

    (void) escm_procedure_new(e, T("open-input-string"), 1, 1,
                              escm_open_input_string, NULL);
    (void) escm_procedure_new(e, T("open-output-string"), 0, 0,
                              escm_open_output_string, NULL);
    (void) escm_procedure_new(e, T("get-output-string"), 1, 1,
                              escm_get_output_string, NULL);

/*    escm_library_exit(e); */
#endif
/*    (void) escm_library_enter(e, "srfi-23", 1); */

    (void) escm_procedure_new(e, T("error"), 1, -1, escm_srfi_error, NULL);

/*    escm_library_exit(e); */
}

/* srfi 2 */

escm_atom *
escm_and_let_star(escm *e, escm_atom *args, void *data)
{
    escm_atom *claws, *arg, *prevenv, *val;
    escm_cons *c;

    (void) data;

    claws = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(claws), claws, e);

    /* we create the new environment and enter it */
    prevenv = escm_env_enter(e, escm_env_new(e, e->env));

    val = e->TRUE;
    /* the we parse each claw and bind each variable */
    while ((arg = escm_cons_pop(e, &claws))) {
        if (ESCM_ISSYM(arg)) {
            val = escm_atom_eval(e, arg);
            if (!ESCM_ISTRUE(e, val))
                goto falseret;
        } else {
            escm_assert1(ESCM_ISCONS(arg), arg, e,
                         escm_env_leave(e, prevenv));
            c = escm_cons_val(arg);
            escm_assert1(c->cdr == e->NIL || escm_cons_val(c->cdr)->cdr ==
                         e->NIL, arg, e, escm_env_leave(e, prevenv));
            if (c->cdr == e->NIL) {
                val = escm_atom_eval(e, c->car);
                if (!ESCM_ISTRUE(e, val))
                    goto falseret;
            } else {
                escm_assert1(ESCM_ISSYM(c->car), c->car, e,
                             escm_env_leave(e, prevenv));
                val = escm_atom_eval(e, escm_cons_val(c->cdr)->car);
                if (ESCM_ISCLOSURE(val) && !escm_proc_val(val)->name)
                    escm_proc_val(val)->name = tcsdup(escm_sym_name(c->car));
                escm_env_set(e, e->env, c->car, val);
            }
        }
    }

    if (escm_cons_val(args) != NULL)
        val = escm_and(e, args, NULL);
    escm_env_leave(e, prevenv);
    return val;

falseret:
    escm_env_leave(e, prevenv);
    return e->FALSE;
}

#if defined ESCM_USE_STRINGS && defined ESCM_USE_PORTS
/* srfi 6 */
escm_atom *
escm_open_input_string(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_STRING)) {
        escm_error(e, _(T("~s: string type is off.~%")), escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
        escm_error(e, _(T("~s: port type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

     return escm_port_make(e, escm_input_str(escm_str_val(str)), 1);
}

escm_atom *
escm_open_output_string(escm *e, escm_atom *args, void *nil)
{
    (void) args;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_STRING)) {
        escm_error(e, _(T("~s: string type is off.~%")), escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
        escm_error(e, _(T("~s: port type is off.~%")), escm_fun(e));
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
        escm_error(e, _(T("~s: string type is off.~%")), escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_PORT)) {
        escm_error(e, _(T("~s: port type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    port = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(port), port, e);

    if (escm_port_val(port)->input) {
        escm_error(e, _(T("~s: given port is not an output port.~%")), escm_fun(e));
        escm_abort(e);
    }

    outp = escm_port_val(port)->d.output;

    return escm_string_make(e, escm_output_getstr(outp),
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

    escm_printf(e->errp, _(T("error: ")));
    escm_atom_print4(e, reason, e->errp, 1);
    if (args != e->NIL)
        escm_printf(e->errp, T(": "));
    while (args != e->NIL) {
        reason = escm_cons_pop(e, &args);
        escm_atom_print4(e, reason, e->errp, 1);
        escm_putc(e->errp, T(' '));
    }
    escm_putc(e->errp, T('\n'));

    escm_abort(e);
}
