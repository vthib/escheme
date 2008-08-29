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

#ifdef ESCM_USE_UNICODE
# define ESCM_EOF WEOF
#else
# define ESCM_EOF EOF
#endif

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
#ifdef ESCM_USE_UNICODE
            wint_t *ub;
#else
            int *ub;
#endif
            size_t usize;
            size_t un;
        } file;
        struct {
#ifdef ESCM_USE_UNICODE
            wchar_t *str;
            wchar_t *cur;
#else
            char *str;
            char *cur;
#endif
        } str;
    } d;

    unsigned int managed : 1;
    unsigned int end : 1;
    unsigned int type : 1;
};

escm_input *escm_input_fopen(const char *);
escm_input *escm_input_fmng(FILE *, const char *);
escm_input *escm_input_str(const escm_char *);

char *escm_input_gettext(escm_input *, const char *);
char *escm_input_getstr_fun(escm_input *, int (*)(int), int);

void escm_input_close(escm_input *);

void escm_input_rewind(escm_input *);

void escm_input_print(escm_input *, escm_output *);

escm_int escm_input_getc(escm_input *);
escm_int escm_input_peek(escm_input *);
void escm_input_ungetc(escm_input *, escm_int);

#ifdef ESCM_USE_UNICODE
wchar_t *escm_input_getwtext(escm_input *, const wchar_t *);
wchar_t *escm_input_getwstr_fun(escm_input *, int (*)(wint_t), int);
#endif

#endif /* ESCHEME_INPUT_H */
