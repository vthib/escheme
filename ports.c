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
#include <stdio.h>

#include "escheme.h"

static size_t porttype = 0;

static void port_free(escm_port *);
static void port_exit(escm *, escm_curports *);
static void port_print(escm *, escm_port *, escm_output *, int);

struct escm_curports {
    escm_atom *input;
    escm_atom *output;
};

void
escm_ports_init(escm *e)
{
    escm_type *t;
    escm_curports *cp;

    cp = xmalloc(sizeof *cp);

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) port_free;
    t->fprint = (Escm_Fun_Print) port_print;
    t->fexit = (Escm_Fun_Exit) port_exit;
    t->dexit = cp;

    porttype = escm_type_add(e, t);

    cp->input = escm_port_make(e, e->input, 1);
    escm_port_val(cp->input)->nofree = 1;
    escm_gc_gard(e, cp->input);

    cp->output = escm_port_make(e, e->output, 0);
    escm_port_val(cp->output)->nofree = 1;
    escm_gc_gard(e, cp->output);

    (void) escm_procedure_new(e, "port?", 1, 1, escm_port_p, NULL);
    (void) escm_procedure_new(e, "input-port?", 1, 1, escm_input_port_p, NULL);
    (void) escm_procedure_new(e, "output-port?", 1, 1, escm_output_port_p,
			      NULL);

    (void) escm_procedure_new(e, "current-input-port", 0, 0,
			      (Escm_Fun_Prim) escm_current_input_port, cp);
    (void) escm_procedure_new(e, "current-output-port", 0, 0,
			      (Escm_Fun_Prim) escm_current_output_port, cp);

    (void) escm_procedure_new(e, "with-input-from-file", 2, 2,
			      (Escm_Fun_Prim) escm_with_input_from_file, cp);
    (void) escm_procedure_new(e, "with-output-to-file", 2, 2,
			      (Escm_Fun_Prim) escm_with_output_to_file, cp);

    (void) escm_procedure_new(e, "open-input-file", 1, 1, escm_open_input_file,
			      NULL);
    (void) escm_procedure_new(e, "open-output-file", 1, 1,
			      escm_open_output_file, NULL);

    (void) escm_procedure_new(e, "close-input-port", 1, 1, escm_close_port,
			      NULL);
    (void) escm_procedure_new(e, "close-output-port", 1, 1, escm_close_port,
			      NULL);

#ifdef ESCM_USE_PORTS
    (void) escm_procedure_new(e, "read", 0, 1, (Escm_Fun_Prim) escm_read, cp);
# ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, "read-char", 0, 1,
			      (Escm_Fun_Prim) escm_read_char, cp);
    (void) escm_procedure_new(e, "peek-char", 0, 1,
			      (Escm_Fun_Prim) escm_peek_char, cp);
# endif

    (void) escm_procedure_new(e, "write", 1, 2, (Escm_Fun_Prim) escm_write, cp);
    (void) escm_procedure_new(e, "display", 1, 2, (Escm_Fun_Prim) escm_display,
			      cp);
    (void) escm_procedure_new(e, "newline", 0, 1, (Escm_Fun_Prim) escm_newline,
			      cp);
# ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, "write-char", 1, 2,
			      (Escm_Fun_Prim) escm_write_char, cp);
# endif
#endif
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
escm_port_p(escm *e, escm_atom *args)
{
    return (ESCM_ISPORT(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_input_port_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(a), a, e);

    return (escm_port_val(a)->input) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_output_port_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(a), a, e);

    return (escm_port_val(a)->input) ? e->FALSE : e->TRUE;
}

escm_atom *
escm_current_input_port(escm *e, escm_atom *args, escm_curports *cp)
{
    (void) e;
    (void) args;

    return cp->input;
}

escm_atom *
escm_current_output_port(escm *e, escm_atom *args, escm_curports *cp)
{
    (void) e;
    (void) args;

    return cp->output;
}

escm_atom *
escm_with_input_from_file(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *str, *thunk, *save;
    escm_input *input;

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    thunk = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(thunk), thunk, e);

    input = escm_input_fopen(escm_str_val(str));
    if (!input) {
	e->err = -1;
	return NULL;
    }

    save = cp->input;
    cp->input = escm_port_make(e, input, 1);
    escm_gc_gard(e, cp->input);

    str = escm_procedure_exec(e, thunk, e->NIL, 0);

    escm_gc_ungard(e, cp->input);
    cp->input = save;

    return str;
}

escm_atom *
escm_with_output_to_file(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *str, *thunk, *save;
    escm_output *o;

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    thunk = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPROC(thunk), thunk, e);

    o = escm_output_fopen(escm_str_val(str));
    if (!o) {
	e->err = -1;
	return NULL;
    }

    save = cp->output;
    cp->output = escm_port_make(e, o, 0);
    escm_gc_gard(e, cp->output);

    str = escm_procedure_exec(e, thunk, e->NIL, 0);

    escm_gc_ungard(e, cp->output);
    cp->output = save;

    return str;
}

escm_atom *
escm_open_input_file(escm *e, escm_atom *args)
{
    escm_input *inp;
    escm_atom *name;

    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(name), name, e);

    inp = escm_input_fopen(escm_str_val(name));
    if (!inp) {
	e->err = -1;
	return NULL;
    }

    return escm_port_make(e, inp, 1);
}

escm_atom *
escm_open_output_file(escm *e, escm_atom *args)
{
    escm_output *outp;
    escm_atom *name;

    name = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(name), name, e);

    outp = escm_output_fopen(escm_str_val(name));
    if (!outp) {
	e->err = -1;
	return NULL;
    }

    return escm_port_make(e, outp, 0);
}

escm_atom *
escm_close_port(escm *e, escm_atom *args)
{
    escm_atom *port;

    port = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISPORT(port), port, e);

    escm_port_val(port)->closed = 1;
    return NULL;
}

#ifdef ESCM_USE_PORTS
escm_atom *
escm_read(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *a;
    escm_port *port;
    escm_input *save;

    a = escm_cons_pop(e, &args);
    if (a)
	escm_assert(ESCM_ISPORT(a), a, e);
    port = (a) ? escm_port_val(a) : escm_port_val(cp->input);

    if (!port->input) {
	fprintf(stderr, "read: given port is not an input port.\n");
	e->err = -1;
	return NULL;
    }
    if (port->closed) {
	fprintf(stderr, "read: port is closed.\n");
	e->err = -1;
	return NULL;
    }

    save = e->input;
    e->input = port->d.input;
    a = escm_parse(e);
    e->input = save;

    return a;
}

# ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_read_char(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *a;
    escm_port *port;
    int c;

    a = escm_cons_pop(e, &args);
    if (a)
	escm_assert(ESCM_ISPORT(a), a, e);
    port = (a) ? escm_port_val(a) : escm_port_val(cp->input);

    if (!port->input) {
	fprintf(stderr, "read-char: given port is not an input port.\n");
	e->err = -1;
	return NULL;
    }
    if (port->closed) {
	fprintf(stderr, "read-char: port is closed.\n");
	e->err = -1;
	return NULL;
    }

    c = escm_input_getc(port->d.input);
    if (c == EOF)
	return e->EOF_OBJ;

    return escm_char_make(e, c);
}

escm_atom *
escm_peek_char(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *a;
    escm_port *port;
    int c;

    a = escm_cons_pop(e, &args);
    if (a)
	escm_assert(ESCM_ISPORT(a), a, e);
    port = (a) ? escm_port_val(a) : escm_port_val(cp->input);

    if (!port->input) {
	fprintf(stderr, "peak-char: given port is not an input port.\n");
	e->err = -1;
	return NULL;
    }
    if (port->closed) {
	fprintf(stderr, "peak-char: port is closed.\n");
	e->err = -1;
	return NULL;
    }

    c = escm_input_getc(port->d.input);
    if (c == EOF)
	return e->EOF_OBJ;

    escm_input_ungetc(port->d.input, c);
    return escm_char_make(e, c);
}

# endif /* ESCM_USE_CHARACTERS */

escm_atom *
escm_write(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *atom, *p;
    escm_port *port;

    atom = escm_cons_pop(e, &args);

    p = escm_cons_pop(e, &args);
    if (p)
	escm_assert(ESCM_ISPORT(p), p, e);
    port = (p) ? escm_port_val(p) : escm_port_val(cp->output);

    if (port->input) {
	fprintf(stderr, "write: given port is not an output port.\n");
	e->err = -1;
	return NULL;
    }
    if (port->closed) {
	fprintf(stderr, "write: port is closed.\n");
	e->err = -1;
	return NULL;
    }

    escm_atom_print4(e, atom, port->d.output, 0);

    return NULL;
}

escm_atom *
escm_display(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *atom, *p;
    escm_port *port;

    atom = escm_cons_pop(e, &args);

    p = escm_cons_pop(e, &args);
    if (p)
	escm_assert(ESCM_ISPORT(p), p, e);
    port = (p) ? escm_port_val(p) : escm_port_val(cp->output);

    if (port->input) {
	fprintf(stderr, "write: given port is not an output port.\n");
	e->err = -1;
	return NULL;
    }
    if (port->closed) {
	fprintf(stderr, "write: port is closed.\n");
	e->err = -1;
	return NULL;
    }

    escm_atom_print4(e, atom, port->d.output, 1);

    return NULL;
}

escm_atom *
escm_newline(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *p;
    escm_port *port;

    p = escm_cons_pop(e, &args);
    if (p)
	escm_assert(ESCM_ISPORT(p), p, e);
    port = (p) ? escm_port_val(p) : escm_port_val(cp->output);

    if (port->input) {
	fprintf(stderr, "newline: given port is not an output port.\n");
	e->err = -1;
	return NULL;
    }
    if (port->closed) {
	fprintf(stderr, "newline: port is closed.\n");
	e->err = -1;
	return NULL;
    }

    escm_putc(port->d.output, '\n');

    return NULL;
}

# ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_write_char(escm *e, escm_atom *args, escm_curports *cp)
{
    escm_atom *c, *p;
    escm_port *port;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    p = escm_cons_pop(e, &args);
    if (p)
	escm_assert(ESCM_ISPORT(p), p, e);
    port = (p) ? escm_port_val(p) : escm_port_val(cp->output);

    if (port->input) {
	fprintf(stderr, "write-char: given port is not an output port.\n");
	e->err = -1;
	return NULL;
    }
    if (port->closed) {
	fprintf(stderr, "write-char: port is closed.\n");
	e->err = -1;
	return NULL;
    }

    escm_putc(port->d.output, escm_char_val(c));

    return NULL;
}
# endif /* ESCM_USE_CHARACTERS */
#endif /* ESCM_USE_PORTS */

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
port_exit(escm *e, escm_curports *cp)
{
    escm_gc_ungard(e, cp->output);
    escm_gc_ungard(e, cp->input);

    free(cp), cp = NULL;
}

static void
port_print(escm *e, escm_port *port, escm_output *stream, int lvl)
{
    (void) e;
    (void) lvl;

    if (port->input)
	escm_printf(stream, "#<input-port %s>",
		    (port->d.input->type == INPUT_FILE) ?
		    port->d.input->d.file.name : "(string)");
    else
	escm_printf(stream, "#<output-port %s>",
		    (port->d.output->type == OUTPUT_FILE) ?
		    port->d.output->d.file.name : "(string)");
}
