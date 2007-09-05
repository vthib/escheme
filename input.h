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
#ifndef ESCHEME_INPUT_H
# define ESCHEME_INPUT_H

#include "types.h"

enum { INPUT_FILE, INPUT_STR };

struct escm_input {
    union {
	struct {
	    long line; /* a better type would be "size_t", but in some case we
			  need to use "-1", and ssize_t isn't standard */
	    long car;

	    FILE *fp;
	    char *name;

	    /* used to allow multiples calls to ungetc */
	    char *ub;
	    size_t usize;
	    size_t un;
	} file;
	struct {
	    const char *str;
	    char *cur;
	} str;
    } d;

    unsigned int end : 1;
    unsigned int type : 1;
};

escm_input *escm_input_fopen(const char *, const char *);
escm_input *escm_input_fmng(FILE *, const char *);
escm_input *escm_input_str(const char *);

void escm_input_close(escm_input *);

int escm_input_getc(escm_input *);
char *escm_input_gettext(escm_input *, const char *);
char *escm_input_getsymbol(escm_input *);
char *escm_input_getstr_fun(escm_input *, int (*)(int));

int escm_input_getint(escm_input *);

void escm_input_rewind(escm_input *);
void escm_input_pushback(escm_input *, size_t);
void escm_input_ungetc(escm_input *, int);

void escm_input_badend(escm_input *);
void escm_input_print(escm_input *, const char *, ...);

#endif /* ESCHEME_INPUT_H */
