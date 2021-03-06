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
#ifndef ESCHEME_PORTS_H
# define ESCHEME_PORTS_H

#ifdef ESCM_USE_PORTS

# ifndef ESCM_USE_STRINGS
#  error "\"ports\" requires the \"string\" implementation."
# endif

# include "escm.h"

# define ESCM_TYPE_PORT escm_port_tget()

# define ESCM_ISPORT(x) ((x)->type == ESCM_TYPE_PORT)

# define escm_port_val(x) ((escm_port *) (x)->ptr)

typedef struct escm_port {
    union {
        escm_input *input;
        escm_output *output;
    } d;

    unsigned int nofree : 1;
    unsigned int closed : 1;
    unsigned int input : 1;
} escm_port;

void escm_ports_init(escm *);
size_t escm_port_tget(void);
escm_atom *escm_port_make(escm *, void *, int);

escm_atom *escm_port_p(escm *, escm_atom *, void *);
escm_atom *escm_input_port_p(escm *, escm_atom *, void *);
escm_atom *escm_output_port_p(escm *, escm_atom *, void *);

escm_atom *escm_current_input_port(escm *, escm_atom *, void *);
escm_atom *escm_current_output_port(escm *, escm_atom *, void *);
escm_atom *escm_current_error_port(escm *, escm_atom *, void *);

escm_atom *escm_with_input_from_file(escm *, escm_atom *, void *);
escm_atom *escm_with_output_to_file(escm *, escm_atom *, void *);
escm_atom *escm_with_error_to_file(escm *, escm_atom *, void *);

escm_atom *escm_open_input_file(escm *, escm_atom *, void *);
escm_atom *escm_open_output_file(escm *, escm_atom *, void *);

escm_atom *escm_close_port(escm *, escm_atom *, void *);

#endif /* ESCM_USE_PORTS */

#endif /* ESCHEME_PORTS_H */
