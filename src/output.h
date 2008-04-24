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
#ifdef ESCM_USE_UNICODE
	    wchar_t *str;
	    wchar_t *cur;
#else
	    char *str;
	    char *cur;
#endif
	    size_t maxlen;
	} str;
    } d;

    unsigned int type : 1;
};

escm_output *escm_output_fopen(const char *);
escm_output *escm_output_fmng(FILE *, const char *);

escm_output *escm_output_str(void);
#ifdef ESCM_USE_UNICODE
wchar_t *escm_output_getstr(escm_output *);
#else
char *escm_output_getstr(escm_output *);
#endif

void escm_output_close(escm_output *);

void escm_vprintf(escm_output *, const char *, va_list);
void escm_printf(escm_output *, const char *, ...);
void escm_parse_print(escm_input *, escm_output *, const char *, ...);
void escm_scmpf(escm *, escm_output *, const char *, ...);

void escm_notice(escm *, const char *, ...);
void escm_warning(escm *, const char *, ...);
void escm_error(escm *, const char *, ...);

void escm_print_slashify(escm_output *, const char *);

#ifdef ESCM_USE_UNICODE
# define escm_putwc(o, c) escm_printf(o, "%lc", c)

void escm_print_wslashify(escm_output *, const wchar_t *);

# define escm_putc escm_putwc
#else
void escm_putc(escm_output *, int);
#endif /* ESCM_USE_UNICODE */

#endif /* ESCHEME_OUTPUT_H */
