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
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include "utils.h"
#include "input.h"

enum { INPUT_FILE, INPUT_STR };

#define MAX_BUFFSIZE 4096

static char strbuf[MAX_BUFFSIZE];

/**
 * @brief open `name' with the given rights
 */
escm_input *
escm_input_fopen(const char *name, const char *mode)
{
    escm_input *f;

    assert(name != NULL);
    assert(mode != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_FILE;

    f->d.file.fp = fopen(name, mode);
    if (!f->d.file.fp) {
	perror(name);
	free(f);
	f = NULL;
    } else {
	f->d.file.name = xstrdup(name);
	f->d.file.line = 1;
    }

    return f;
}

/**
 * @brief manage a file pointer
 */
escm_input *
escm_input_fmng(FILE *fp, const char *name)
{
    escm_input *f;

    assert(fp != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_FILE;
    f->d.file.fp = fp;

    f->d.file.name = xstrdup(name);
    f->d.file.line = f->d.file.car = -1; /* signal that we can't know where we
					    are in the stream */

    return f;
}

/**
 * @brief set the string as input
 */
escm_input *
escm_input_str(const char *str)
{
    escm_input *f;

    assert(str != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_STR;

    f->d.str.str = str;
    f->d.str.cur = (char *) f->d.str.str;

    return f;
}

/**
 * @brief close the input and free it
 */
void
escm_input_close(escm_input *f)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
	if (EOF == fclose(f->d.file.fp))
	    perror("fclose");
	free(f->d.file.name);
	free(f->d.file.ub);
    }

    free(f);
}

/**
 * @brief return the next char in the stream
 */
int
escm_input_getc(escm_input *f)
{
    int c;

    assert(f != NULL);

    if (f->type == INPUT_FILE) {
	if (f->d.file.un > 0)
	    c = f->d.file.ub[--f->d.file.un];
	else
	    c = getc(f->d.file.fp);

	if (c == EOF)
	    f->end = 1;
	else if (c == '\n') {
	    if (f->d.file.line != -1)
		f->d.file.line++;
	    f->d.file.car = 0;
	} else {
	    if (f->d.file.car != -1)
		f->d.file.car++;
	}
    } else {
	c = (int) (f->end) ? '\0' : *f->d.str.cur++;
	if (*f->d.str.cur == '\0')
	    f->end = 1;
    }

    return c;
}

/**
 * @brief return a text that end with one of the `end' chars
 */
char *
escm_input_gettext(escm_input *f, const char *end)
{
    size_t len = 0;
    int c;

    assert(f != NULL);
    assert(end != NULL);

    do {
	c = escm_input_getc(f);
	if (c == '\\') {
	    c = escm_input_getc(f);
	    switch (c) {
	    case 'a': c = '\a'; break;
	    case 'b': c = '\b'; break;
	    case 'f': c = '\f'; break;
	    case 'n': c = '\n'; break;
	    case 'r': c = '\r'; break;
	    case 't': c = '\t'; break;
	    case 'v': c = '\v'; break;
	    case '"':
		strbuf[len++] = c;
		c = escm_input_getc(f);
		break;
	    default: break; /* keep the new character */
	    }
	}
	strbuf[len++] = c;
    } while (c != EOF && !strchr(end, c));

    escm_input_ungetc(f, c);
    strbuf[len - 1] = '\0';

    return xstrdup(strbuf);
}

/* obsolete? */
char *
escm_input_getsymbol(escm_input *f)
{
    size_t len = 0;
    int c;
    long save;

    assert(f != NULL);

    save = (f->type == INPUT_FILE) ? f->d.file.car : 0;

    do {
	c = escm_input_getc(f);
	strbuf[len++] = c;
    } while (c != EOF &&
	     (strchr("!$%&*+-./:<=>?@^_~", c) != NULL || isalnum(c)));

    escm_input_ungetc(f, c);
    if (c == '\n' && save)
	f->d.file.car = save + len - 1;

    strbuf[len - 1] = '\0';

    return xstrdup(strbuf);
}

/**
 * @brief get a string. Each character is passed to "fun" which must return 1
 * if the character is valid, 0 else (cf ctype.h)
 */
char *
escm_input_getstr_fun(escm_input *f, int (*fun)(int))
{
    size_t len = 0;
    int c;

    assert(f != NULL);

    do {
	c = escm_input_getc(f);
	strbuf[len++] = c;
    } while (c != EOF && fun(c));

    escm_input_ungetc(f, c);
    strbuf[len - 1] = '\0';

    return xstrdup(strbuf);
}

/**
 * @brief get an int from the input
 */
int
escm_input_getint(escm_input *file)
{
    int c;
    int i = 0;
    int positive = 1;

    assert(file != NULL);

    c = escm_input_getc(file);
    if (c == '-') {
	positive = 0;
	c = escm_input_getc(file);
    } else if (c == '+')
	c = escm_input_getc(file);

    for (; c && isdigit(c); c = escm_input_getc(file))
	i *= 10, i += (c - '0');

    escm_input_ungetc(file, c);

    return i;
}

/**
 * @brief get a char from the input ("space" is ' ' and "newline" is '\n')
 */
char
escm_input_getchar(escm_input *file)
{
    char *str;
    char c;
    size_t len;

    assert(file != NULL);

    str = escm_input_getstr_fun(file, isalpha);
    len = strlen(str);

    if (len < 1) {
	escm_input_print(file, "character missing after #\\.");
	c = '\0';
    } else if (len == 1)
	c = *str;
    else {
	char *p;

	for (p = str; *p; p++)
	    *p = tolower(*p);
	if (strcmp(str, "newline") == 0)
	    c = '\n';
	else if (strcmp(str, "space") == 0)
	    c = ' ';
	else {
	    escm_input_print(file, "unknown character #\\%s.", str);
	    c = '\0';
	}
    }

    free(str);
    return c;
}   

/**
 * @brief rewind the input structure
 */
void
escm_input_rewind(escm_input *f)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
	rewind(f->d.file.fp);
	f->d.file.line = 0;
	f->d.file.car = 0;
    } else
	f->d.str.cur = (char *) f->d.str.str;

    f->end = 0;
}

/**
 * @brief push `n' character back in the input
 */
void
escm_input_pushback(escm_input *f, size_t n)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
	if (f->d.file.car != -1) {
	    if (f->d.file.car < (long) n) {
		f->d.file.car = 0;
		f->d.file.line--;
	    } else
		f->d.file.car -= n;
	}

	if (-1 == fseek(f->d.file.fp, -((long) n), SEEK_CUR))
	    perror("fseek");
    } else {
	f->d.str.cur -= n;
	if (f->d.str.cur < f->d.str.str)
	    f->d.str.cur = f->d.str.cur;
    }

    if (n > 0)
	f->end = 0;
}

/**
 * @brief put a character back in the input
 */
void
escm_input_ungetc(escm_input *f, int c)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
	if (f->d.file.car != -1) {
	    if (f->d.file.car > 0)
		f->d.file.car--;
	    else if (f->d.file.car == 0 && c == '\n' && f->d.file.line != -1)
		f->d.file.line--;
	}

#if 0
	if (EOF == ungetc(c, f->d.file.fp)) {
	    fprintf(stderr, "ungetc failed.\n");
	    return;
	}
#endif
	if (f->d.file.usize <= f->d.file.un) {
	    f->d.file.usize += 2;
	    f->d.file.ub = xrealloc(f->d.file.ub, f->d.file.usize);
	}
	f->d.file.ub[f->d.file.un++] = (char) c;
    } else {
	if (f->d.str.cur > f->d.str.str)
	    f->d.str.cur--;
    }

    f->end = 0;
}

/**
 * @brief just print a warning
 */
void
escm_input_badend(escm_input *f)
{
    assert(f != NULL);

    f->end = 1;
    escm_input_print(f, "unexpected EOF.");
}

/**
 * @brief print a message on standard output in the form
 * "filename:line:car: message\n" or "str: message"
 *                                     ^
 */
void
escm_input_print(escm_input *f, const char *s, ...)
{
    va_list va;

    assert(f != NULL);
    assert(s != NULL);

    va_start(va, s);

    if (f->type == INPUT_FILE) {
	fprintf(stderr, "%s:", f->d.file.name);
	if (f->d.file.line == -1)
	    fprintf(stderr, ":");
	else
	    fprintf(stderr, "%ld:", f->d.file.line);
	if (f->d.file.car == -1)
	    fprintf(stderr, ": ");
	else
	    fprintf(stderr, "%ld: ", f->d.file.car);

	(void) vfprintf(stderr, s, va);
	fprintf(stderr, "\n");
    } else {
	char *p;

	fprintf(stderr, "%s: ", f->d.str.str);
	(void) vfprintf(stderr, s, va);
	fprintf(stderr, "\n");

	for (p = (char *) f->d.str.str; p < f->d.str.cur; p++)
	    fprintf(stderr, " ");
	fprintf(stderr, "^\n");
    }

    va_end(va);
}
