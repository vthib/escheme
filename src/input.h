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

            int *ub;

            size_t usize;
            size_t un;
        } file;
        struct {
            tchar *str;
            tchar *cur;
        } str;
    } d;

    unsigned int managed : 1;
    unsigned int end : 1;
    unsigned int type : 1;
};

escm_input *escm_input_fopen(const char *);
escm_input *escm_input_fopen_prefixed(const char *, escm_input *);
escm_input *escm_input_fmng(FILE *, const char *);
escm_input *escm_input_str(const tchar *);

tchar *escm_input_gettext(escm_input *, const tchar *);
tchar *escm_input_getstr_fun(escm_input *, int (*)(tint), int);

void escm_input_close(escm_input *);

void escm_input_rewind(escm_input *);

void escm_input_print(escm_input *, escm_output *);

tint escm_input_getc(escm_input *);
tint escm_input_peek(escm_input *);
void escm_input_ungetc(escm_input *, tint);

#endif /* ESCHEME_INPUT_H */
