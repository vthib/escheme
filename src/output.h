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
#ifndef ESCHEME_OUTPUT_H
# define ESCHEME_OUTPUT_H

#include <stdio.h>
#include <stdarg.h>

#include "types.h"

enum { OUTPUT_FILE, OUTPUT_STR };

struct escm_output {
    union {
        struct {
            char *name;
            FILE *fp;
        } file;
        struct {
            tchar *str;
            tchar *cur;
            size_t maxlen;
        } str;
    } d;

    unsigned int type : 1;
};

escm_output *escm_output_fopen(const char *);
escm_output *escm_output_fmng(FILE *, const char *);

escm_output *escm_output_str(void);
tchar *escm_output_getstr(escm_output *);

void escm_output_close(escm_output *);

void escm_vprintf(escm_output *, const tchar *, va_list);
void escm_printf(escm_output *, const tchar *, ...);
void escm_parse_print(escm_input *, escm_output *, const tchar *, ...);
void escm_scmpf(escm *, escm_output *, const tchar *, ...);
void escm_scmpf2(escm *, escm_output *, const tchar *, escm_atom *);

void escm_notice(escm *, const tchar *, ...);
void escm_warning(escm *, const tchar *, ...);
void escm_error(escm *, const tchar *, ...);

void escm_print_slashify(escm_output *, const tchar *);

void escm_putc(escm_output *, tint);

#endif /* ESCHEME_OUTPUT_H */
