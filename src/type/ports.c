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

#include "ports.h"
#include "base.h"
#include "strings.h"

static size_t porttype = 0;

static void port_free(escm_port *);
static void port_print(escm *, escm_port *, escm_output *, int);

void
escm_ports_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) port_free;
    t->print.fprint = (Escm_Fun_Print) port_print;

    porttype = escm_type_add(e, t);

    (void) escm_procedure_new(e, T("port?"), 1, 1, escm_port_p, NULL);
    (void) escm_procedure_new(e, T("input-port?"), 1, 1, escm_input_port_p, NULL);
    (void) escm_procedure_new(e, T("output-port?"), 1, 1, escm_output_port_p,
                              NULL);

    (void) escm_procedure_new(e, T("current-input-port"), 0, 0,
                              (Escm_Fun_Prim) escm_current_input_port, NULL);
    (void) escm_procedure_new(e, T("current-output-port"), 0, 0,
                              (Escm_Fun_Prim) escm_current_output_port, NULL);
    (void) escm_procedure_new(e, T("current-error-port"), 0, 0,
                              (Escm_Fun_Prim) escm_current_error_port, NULL);

    (void) escm_procedure_new(e, T("with-input-from-file"), 2, 2,
                              (Escm_Fun_Prim) escm_with_input_from_file, NULL);
    (void) escm_procedure_new(e, T("with-output-to-file"), 2, 2,
                              (Escm_Fun_Prim) escm_with_output_to_file, NULL);

    (void) escm_procedure_new(e, T("open-input-file"), 1, 1, escm_open_input_file,
                              NULL);
    (void) escm_procedure_new(e, T("open-output-file"), 1, 1,
                              escm_open_output_file, NULL);

    (void) escm_procedure_new(e, T("close-input-port"), 1, 1, escm_close_port,
                              NULL);
    (void) escm_procedure_new(e, T("close-output-port"), 1, 1, escm_close_port,
                              NULL);

}

size_t
escm_port_tget(void)
{
    return porttype;
}

escm_atom *
escm_port_make(escm *e, void *ptr, int input)
{
    escm_port *p;

    p = xcalloc(1, sizeof *p);
    if (input)
        p->d.input = ptr;
    else
        p->d.output = ptr;

    p->input = !!input;
    p->closed = 0;

    return escm_atom_new(e, porttype, p);
}

escm_atom *
escm_port_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return (ESCM_ISPORT(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_input_port_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(a), a, e);

    return (escm_port_val(a)->input) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_output_port_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(a), a, e);

    return (escm_port_val(a)->input) ? e->FALSE : e->TRUE;
}

escm_atom *
escm_current_input_port(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) args; (void) nil;
    return escm_port_make(e, e->input, 1);
}

escm_atom *
escm_current_output_port(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) args; (void) nil;
    return escm_port_make(e, e->output, 0);
}

escm_atom *
escm_current_error_port(escm *e, escm_atom *args, void *nil)
{
    (void) e; (void) args; (void) nil;
    return escm_port_make(e, e->errp, 0);
}

escm_atom *
escm_with_input_from_file(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *thunk;
    escm_input *input, *save;

    (void) nil;
    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    thunk = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(thunk), thunk, e);

#ifdef ESCM_UNICODE
    {
        char *s;

        s = tcstostr(escm_str_val(str));
        input = escm_input_fopen(s);
        free(s);
    }
#else
        input = escm_input_fopen(escm_str_val(str));
#endif

    if (!input)
        escm_abort(e);

    save = e->input;
    e->input = input;

    str = escm_procedure_exec(e, thunk, e->NIL, 0);

    e->input = save;
    escm_input_close(input);

    return str;
}

escm_atom *
escm_with_output_to_file(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *thunk;
    escm_output *save, *o;

    (void) nil;
    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    thunk = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(thunk), thunk, e);

#ifdef ESCM_UNICODE
    {
        char *s;

        s = tcstostr(escm_str_val(str));
        o = escm_output_fopen(s);
        free(s);
    }
#else
        o = escm_output_fopen(escm_str_val(str));
#endif

    if (!o)
        escm_abort(e);

    save = e->output;
    e->output = o;

    str = escm_procedure_exec(e, thunk, e->NIL, 0);

    e->output = save;
    escm_output_close(o);

    return str;
}

escm_atom *
escm_with_error_to_file(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *thunk;
    escm_output *save, *o;

    (void) nil;
    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    thunk = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(thunk), thunk, e);

#ifdef ESCM_UNICODE
    {
        char *s;

        s = tcstostr(escm_str_val(str));
        o = escm_output_fopen(s);
        free(s);
    }
#else
        o = escm_output_fopen(escm_str_val(str));
#endif

    if (!o)
        escm_abort(e);

    save = e->errp;
    e->errp = o;

    str = escm_procedure_exec(e, thunk, e->NIL, 0);

    e->errp = save;
    escm_output_close(o);

    return str;
}

escm_atom *
escm_open_input_file(escm *e, escm_atom *args, void *nil)
{
    escm_input *inp;
    escm_atom *name;

    (void) nil;
    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(name), name, e);

#ifdef ESCM_UNICODE
    {
        char *s;

        s = tcstostr(escm_str_val(name));
        inp = escm_input_fopen(s);
        free(s);
    }
#else
    inp = escm_input_fopen(escm_str_val(name));
#endif

    if (!inp)
        escm_abort(e);

    return escm_port_make(e, inp, 1);
}

escm_atom *
escm_open_output_file(escm *e, escm_atom *args, void *nil)
{
    escm_output *outp;
    escm_atom *name;

    (void) nil;
    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(name), name, e);

#ifdef ESCM_UNICODE
    {
        char *s;

        s = tcstostr(escm_str_val(name));
        outp = escm_output_fopen(s);
        free(s);
    }
#else
        outp = escm_output_fopen(escm_str_val(name));
#endif

    if (!outp)
        escm_abort(e);

    return escm_port_make(e, outp, 0);
}

escm_atom *
escm_close_port(escm *e, escm_atom *args, void *nil)
{
    escm_atom *port;

    (void) nil;
    port = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(port), port, e);

    escm_port_val(port)->closed = 1;
    return NULL;
}

static void
port_free(escm_port *port)
{
    if (!port)
        return;

    if (!port->nofree) {
        if (port->input)
            escm_input_close(port->d.input);
        else
            escm_output_close(port->d.output);
    }

    free(port);
}

static void
port_print(escm *e, escm_port *port, escm_output *stream, int lvl)
{
    (void) e;
    (void) lvl;

    if (port->input)
        escm_printf(stream, T("#<input-port %s>"),
                    (port->d.input->type == INPUT_FILE) ?
                    port->d.input->d.file.name : "(string)");
    else
        escm_printf(stream, T("#<output-port %s>"),
                    (port->d.output->type == OUTPUT_FILE) ?
                    port->d.output->d.file.name : "(string)");
}
