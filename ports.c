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
static void port_print(escm *, escm_port *, FILE *);

void
escm_ports_init(escm *e)
{
    escm_type *t;

    t = xmalloc(sizeof *t);
    t->fmark = NULL;
    t->ffree = (Escm_Fun_Free) port_free;
    t->fprint = (Escm_Fun_Print) port_print;
    t->fequal = NULL;
    t->fparsetest = NULL;
    t->fparse = NULL;
    t->feval = NULL;

    porttype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "port?", 1, 1, escm_port_p, NULL);
}

size_t
escm_port_tget(void)
{
    return porttype;
}

escm_atom *
escm_port_p(escm *e, escm_atom *args)
{
    return (ESCM_ISPORT(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

static void
port_free(escm_port *port)
{
    if (!port)
	return;

    free(port->name);
    if (EOF == fclose(port->fp))
	perror("fclose");
    free(port);
}

static void
port_print(escm *e, escm_port *port, FILE *stream)
{
    (void) e;

    fprintf(stream, "#<%s-port %s>", (port->input) ? "input" : "output",
	    port->name);
}
