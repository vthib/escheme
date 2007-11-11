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
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include "utils.h"
#include "output.h"

#ifdef ESCM_USE_C99
# include <wchar.h>
#endif

/**
 * @brief open `name' with the read rights
 */
escm_output *
escm_output_fopen(const char *name)
{
    escm_output *f;

    assert(name != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = OUTPUT_FILE;

    f->d.file.fp = fopen(name, "w");
    if (!f->d.file.fp) {
	perror(name);
	free(f);
	f = NULL;
    } else {
	f->d.file.name = xstrdup(name);
    }

    return f;
}

/**
 * @brief manage a file pointer
 */
escm_output *
escm_output_fmng(FILE *fp, const char *name)
{
    escm_output *f;

    assert(fp != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = OUTPUT_FILE;
    f->d.file.fp = fp;
    f->d.file.name = xstrdup(name);

    return f;
}

/**
 * @brief set the string as output
 */
escm_output *
escm_output_str(void)
{
    escm_output *f;

    f = xcalloc(1, sizeof *f);
    f->type = OUTPUT_STR;

    f->d.str.str = xcalloc(5, sizeof *f->d.str.str);
    f->d.str.cur = (char *) f->d.str.str;
    f->d.str.maxlen = 5;

    return f;
}

char *
escm_output_getstr(escm_output *o)
{
    assert(o != NULL);

    if (o->type == OUTPUT_FILE)
	return NULL;

    *o->d.str.cur = '\0';
    return o->d.str.str;
}

void
escm_output_close(escm_output *f)
{
    if (!f)
	return;

    if (f->type == OUTPUT_FILE) {
	if (EOF == fclose(f->d.file.fp))
	    perror("fclose");
	free(f->d.file.name);
    } else
	free(f->d.str.str);

    free(f);
}

void
escm_printf(escm_output *f, const char *format, ...)
{
    va_list args;

    assert(f != NULL);

    va_start(args, format);

    if (f->type == OUTPUT_FILE)
	(void) vfprintf(f->d.file.fp, format, args);
    else {
	size_t offset;
	int write;

	offset = f->d.str.cur - f->d.str.str;

	for (;;) {
	    f->d.str.maxlen += 30;
	    f->d.str.str = xrealloc(f->d.str.str, f->d.str.maxlen);
	    f->d.str.cur = f->d.str.str + offset;

	    write = vsnprintf(f->d.str.cur, offset - f->d.str.maxlen, format,
			      args);
	    if ((size_t) write < offset - f->d.str.maxlen) {
		f->d.str.cur += write;
		break;
	    } else {
		va_end(args);
		va_start(args, format);
	    }
	}
    }

    va_end(args);
}

void
escm_print_slashify(escm_output *stream, const char *str)
{
    char *p;

    for (p = (char *) str; *p != '\0'; p++) {
	if (*p == '"' || *p == '\\') {
	    escm_putc(stream, '\\');
	}
	escm_putc(stream, *p);
    }
}

#ifdef ESCM_USE_C99
/*
void
escm_putwc(escm_output *f, wint_t c)
{
    assert(f != NULL);

    if (f->type == OUTPUT_FILE) {
	if (WEOF == fputwc(c, f->d.file.fp))
	    fprintf(stderr, "fputwc('%lc') failed.\n", c);
    } else {
	size_t offset;

	*f->d.str.cur++ = c;

	offset = f->d.str.cur - f->d.str.str;

	if (offset >= f->d.str.maxlen) {
	    f->d.str.maxlen += 5;
	    f->d.str.str = xrealloc(f->d.str.str, f->d.str.maxlen);
	    f->d.str.cur = f->d.str.str + offset;
	}
    }
}
*/

void
escm_print_wslashify(escm_output *stream, const wchar_t *str)
{
    size_t i;

    for (i = 0; str[i] != L'\0'; i++) {
	switch (str[i]) {
	case L'"':
	case L'\\':
	    escm_putc(stream, '\\');
	    escm_putc(stream, str[i]);
	    break;
	case L'\a':
	    escm_putc(stream, '\\'); escm_putc(stream, 'a');
	    break;
	case L'\b':
	    escm_putc(stream, '\\'); escm_putc(stream, 'b');
	    break;
	case L'\f':
	    escm_putc(stream, '\\'); escm_putc(stream, 'f');
	    break;
	case L'\n':
	    escm_putc(stream, '\\'); escm_putc(stream, 'n');
	    break;
	case L'\r':
	    escm_putc(stream, '\\'); escm_putc(stream, 'r');
	    break;
	case L'\t':
	    escm_putc(stream, '\\'); escm_putc(stream, 't');
	    break;
	case L'\v':
	    escm_putc(stream, '\\'); escm_putc(stream, 'v');
	    break;
	default:
	    escm_putc(stream, str[i]);
	    break;
	}
    }
}
#else

void
escm_putc(escm_output *f, int c)
{
    assert(f != NULL);

    if (f->type == OUTPUT_FILE) {
	if (EOF == fputc(c, f->d.file.fp))
	    fprintf(stderr, "fputc('%c') failed.\n", c);
    } else {
	size_t offset;

	*f->d.str.cur++ = c;

	offset = f->d.str.cur - f->d.str.str;

	if (offset >= f->d.str.maxlen) {
	    f->d.str.maxlen += 5;
	    f->d.str.str = xrealloc(f->d.str.str, f->d.str.maxlen);
	    f->d.str.cur = f->d.str.str + offset;
	}
    }
}

#endif /* ESCM_USE_C99 */
