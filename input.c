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
#include <ctype.h>

#include "types.h"

#ifdef ESCM_USE_C99
# include <wchar.h>
# include <wctype.h>
#endif

#include "utils.h"
#include "input.h"

#define MAX_BUFFSIZE 2048

static char strbuf[MAX_BUFFSIZE];
#ifdef ESCM_USE_C99
static wchar_t wcsbuf[MAX_BUFFSIZE];
#endif

/**
 * @brief open `name' with the read rights
 */
escm_input *
escm_input_fopen(const char *name)
{
    escm_input *f;

    assert(name != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_FILE;

    f->d.file.fp = fopen(name, "r");
    if (!f->d.file.fp) {
	perror(name);
	free(f);
	f = NULL;
    } else {
	f->d.file.name = xstrdup(name);
	f->d.file.line = 1;
	f->managed = 0;
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
    f->managed = 1;

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
    if (!f)
	return;

    if (f->type == INPUT_FILE) {
	if (!f->managed) {
	    if (EOF == fclose(f->d.file.fp))
		perror("fclose");
	}
	free(f->d.file.name);
	free(f->d.file.ub);
    }

    free(f);
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

    c = escm_input_getc(f);
    while (c != EOF && !strchr(end, c)) {
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
	    case '\\': case '"': break;
	    default:
		strbuf[len++] = '\\';
		break; /* keep the new character */
	    }
	    strbuf[len++] = c;
	} else
	    strbuf[len++] = c;
	c = escm_input_getc(f);
    } 

    if (!f->end)
	escm_input_ungetc(f, c);
    strbuf[len] = '\0';

    return xstrdup(strbuf);
}

/**
 * @brief get a string. Each character is passed to "fun" which must return 1
 * if the character is valid, 0 else (cf ctype.h)
 */
char *
escm_input_getstr_fun(escm_input *f, int (*fun)(int), int casesens)
{
    size_t len = 0;
    int c;

    assert(f != NULL);

    do {
	c = escm_input_getc(f);
	if (!casesens)
	    c = tolower(c);
	strbuf[len++] = c;
    } while (c != EOF && fun(c));

    if (!f->end)
	escm_input_ungetc(f, c);
    strbuf[len - 1] = '\0';

    return xstrdup(strbuf);
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

	for (p = (char *) f->d.str.str; p < (f->d.str.cur - 1); p++)
	    fprintf(stderr, " ");
	fprintf(stderr, "^\n");
    }

    va_end(va);
}

#ifndef ESCM_USE_C99
/**
 * @brief return the next char in the stream
 */
int
escm_input_getc(escm_input *f)
{
    int c;

    assert(f != NULL);

    if (f->end)
	return EOF;

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
	if (*f->d.str.cur == '\0')
	    f->end = 1, c = EOF;
	else {
	    c = *f->d.str.cur++;
	    if (*f->d.str.cur == '\0')
		f->end = 1;
	}
    }

    return c;
}

int
escm_input_peek(escm_input *f)
{
    int c;

    assert(f != NULL);

    if (f->end)
	return EOF;

    if (f->type == INPUT_FILE) {
	if (f->d.file.un > 0)
	    return f->d.file.ub[f->d.file.un - 1];

	c = getc(f->d.file.fp);
	
	if (f->d.file.usize <= f->d.file.un) {
	    f->d.file.usize += 2;
	    f->d.file.ub = xrealloc(f->d.file.ub,
				    f->d.file.usize * sizeof *f->d.file.ub);
	}
	f->d.file.ub[f->d.file.un++] = c;
    } else
	c = *f->d.str.cur;

    return c;
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

	if (f->d.file.usize <= f->d.file.un) {
	    f->d.file.usize += 2;
	    f->d.file.ub = xrealloc(f->d.file.ub,
				    f->d.file.usize * sizeof *f->d.file.ub);
	}
	f->d.file.ub[f->d.file.un++] = c;
    } else {
	if (f->d.str.cur > f->d.str.str)
	    f->d.str.cur--;
    }

    f->end = 0;
}

#else

/**
 * @brief return the next wide char in the stream
 */
wint_t
escm_input_getwc(escm_input *f)
{
    wint_t c;

    assert(f != NULL);

    if (f->end)
	return EOF;

    if (f->type == INPUT_FILE) {
	if (f->d.file.un > 0)
	    c = f->d.file.ub[--f->d.file.un];
	else
	    c = fgetwc(f->d.file.fp);

	if (c == WEOF)
	    f->end = 1;
	else if (c == L'\n') {
	    if (f->d.file.line != -1)
		f->d.file.line++;
	    f->d.file.car = 0;
	} else {
	    if (f->d.file.car != -1)
		f->d.file.car++;
	}
    } else {
	if (*f->d.str.cur == '\0')
	    f->end = 1, c = WEOF;
	else {
	    c = *f->d.str.cur++;
	    if (*f->d.str.cur == '\0')
		f->end = 1;
	}
    }

    return c;
}

wint_t
escm_input_wpeek(escm_input *f)
{
    wint_t c;

    assert(f != NULL);

    if (f->end)
	return WEOF;

    if (f->type == INPUT_FILE) {
	if (f->d.file.un > 0)
	    return f->d.file.ub[f->d.file.un - 1];

	c = fgetwc(f->d.file.fp);
	
	if (f->d.file.usize <= f->d.file.un) {
	    f->d.file.usize += 2;
	    f->d.file.ub = xrealloc(f->d.file.ub,
				    f->d.file.usize * sizeof *f->d.file.ub);
	}
	f->d.file.ub[f->d.file.un++] = c;
    } else
	c = *f->d.str.cur;

    return c;
}

wchar_t *
escm_input_getwtext(escm_input *f, const wchar_t *end)
{
    size_t len = 0;
    wint_t c;

    assert(f != NULL);
    assert(end != NULL);

    c = escm_input_getwc(f);
    while (c != WEOF && !wcschr(end, c)) {
	if (c == L'\\') {
	    c = escm_input_getwc(f);
	    switch (c) {
	    case L'a': c = L'\a'; break;
	    case L'b': c = L'\b'; break;
	    case L'f': c = L'\f'; break;
	    case L'n': c = L'\n'; break;
	    case L'r': c = L'\r'; break;
	    case L't': c = L'\t'; break;
	    case L'v': c = L'\v'; break;
	    case L'\\': case '\"': break;
	    default:
		wcsbuf[len++] = '\\';
		break; /* keep the new character */
	    }
	    wcsbuf[len++] = c;
	} else
	    wcsbuf[len++] = c;
	c = escm_input_getwc(f);
    } 

    if (!f->end)
	escm_input_ungetwc(f, c);
    wcsbuf[len] = L'\0';

    return xwcsdup(wcsbuf);
}

/**
 * @brief get a wide string. Each character is passed to "fun" which must
 * return 1 if the character is valid, 0 else (cf ctype.h)
 */
wchar_t *
escm_input_getwstr_fun(escm_input *f, int (*fun)(wint_t), int casesens)
{
    size_t len = 0;
    wint_t c;

    assert(f != NULL);

    do {
	c = escm_input_getwc(f);
	if (!casesens)
	    c = towlower(c);
	wcsbuf[len++] = (wchar_t) c;
    } while (c != WEOF && fun(c));

    if (!f->end)
	escm_input_ungetwc(f, c);
    wcsbuf[len - 1] = L'\0';

    return xwcsdup(wcsbuf);
}

/**
 * @brief put a wide character back in the input
 */
void
escm_input_ungetwc(escm_input *f, wint_t c)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
	if (f->d.file.car != -1) {
	    if (f->d.file.car > 0)
		f->d.file.car--;
	    else if (f->d.file.car == 0 && c == L'\n' && f->d.file.line != -1)
		f->d.file.line--;
	}

	if (f->d.file.usize <= f->d.file.un) {
	    f->d.file.usize += 2;
	    f->d.file.ub = xrealloc(f->d.file.ub,
				    f->d.file.usize * sizeof *f->d.file.ub);
	}
	f->d.file.ub[f->d.file.un++] = c;
    } else {
	if (f->d.str.cur > f->d.str.str)
	    f->d.str.cur--;
    }

    f->end = 0;
}
#endif
