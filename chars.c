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
#include <string.h>
#include <ctype.h>

#include "escheme.h"

static size_t chartype = 0;

static void char_print(escm *, unsigned char, FILE *);
static int char_equal(escm *, unsigned char, unsigned char, unsigned int);
static int char_parsetest(escm *, int);
static escm_atom *char_parse(escm *);

void
escm_chars_init(escm *e)
{
    escm_type *t;

    t = xmalloc(sizeof *t);
    t->fmark = NULL;
    t->ffree = NULL;
    t->fprint = (Escm_Fun_Print) char_print;
    t->fequal = (Escm_Fun_Equal) char_equal;
    t->fparsetest = char_parsetest;
    t->fparse = char_parse;
    t->feval = NULL;

    chartype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "char?", 1, 1, escm_char_p);

    (void) escm_procedure_new(e, "char=?", 2, 2, escm_char_eq_p);
    (void) escm_procedure_new(e, "char<?", 2, 2, escm_char_lt_p);
    (void) escm_procedure_new(e, "char>?", 2, 2, escm_char_gt_p);
    (void) escm_procedure_new(e, "char<=?", 2, 2, escm_char_le_p);
    (void) escm_procedure_new(e, "char>=?", 2, 2, escm_char_ge_p);

    (void) escm_procedure_new(e, "char-ci=?", 2, 2, escm_char_ci_eq_p);
    (void) escm_procedure_new(e, "char-ci<?", 2, 2, escm_char_ci_lt_p);
    (void) escm_procedure_new(e, "char-ci>?", 2, 2, escm_char_ci_gt_p);
    (void) escm_procedure_new(e, "char-ci<=?", 2, 2, escm_char_ci_le_p);
    (void) escm_procedure_new(e, "char-ci>=?", 2, 2, escm_char_ci_ge_p);

    (void) escm_procedure_new(e, "char-alphabetic?", 1, 1,
			      escm_char_alphabetic_p);
    (void) escm_procedure_new(e, "char-numeric?", 1, 1, escm_char_numeric_p);
    (void) escm_procedure_new(e, "char-whitespace?", 1, 1,
			      escm_char_whitespace_p);
    (void) escm_procedure_new(e, "char-upper-case?", 1, 1,
			      escm_char_upper_case_p);
    (void) escm_procedure_new(e, "char-lower_case?", 1, 1,
			      escm_char_lower_case_p);

    (void) escm_procedure_new(e, "char->integer", 1, 1, escm_char_to_integer);
    (void) escm_procedure_new(e, "integer->char", 1, 1, escm_integer_to_char);

    (void) escm_procedure_new(e, "char-upcase", 1, 1, escm_char_upcase);
    (void) escm_procedure_new(e, "char-downcase", 1, 1, escm_char_downcase);
}

size_t
escm_char_tget(void)
{
    return chartype;
}

escm_atom *
escm_char_make(escm *e, unsigned char c)
{
    return escm_atom_new(e, chartype, (void *) (escm_intptr) c);
}

escm_atom *
escm_char_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    return ESCM_ISCHAR(a) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_eq_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (escm_char_val(c1) == escm_char_val(c2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_lt_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (escm_char_val(c1) < escm_char_val(c2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_gt_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (escm_char_val(c1) > escm_char_val(c2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_le_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (escm_char_val(c1) <= escm_char_val(c2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_ge_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (escm_char_val(c1) >= escm_char_val(c2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_ci_eq_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (tolower(escm_char_val(c1)) == tolower(escm_char_val(c2))) ?
	e->TRUE : e->FALSE;
}

escm_atom *
escm_char_ci_lt_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (tolower(escm_char_val(c1)) < tolower(escm_char_val(c2))) ?
	e->TRUE : e->FALSE;
}

escm_atom *
escm_char_ci_gt_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (tolower(escm_char_val(c1)) > tolower(escm_char_val(c2))) ?
	e->TRUE : e->FALSE;
}

escm_atom *
escm_char_ci_le_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (tolower(escm_char_val(c1)) <= tolower(escm_char_val(c2))) ?
	e->TRUE : e->FALSE;
}

escm_atom *
escm_char_ci_ge_p(escm *e, escm_atom *args)
{
    escm_atom *c1, *c2;

    c1 = escm_cons_pop(e, &args);
    c2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c1), c1, e);
    escm_assert(ESCM_ISCHAR(c2), c2, e);

    return (tolower(escm_char_val(c1)) >= tolower(escm_char_val(c2))) ?
	e->TRUE : e->FALSE;
}

escm_atom *
escm_char_alphabetic_p(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return isalpha(escm_char_val(c)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_numeric_p(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return isdigit(escm_char_val(c)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_whitespace_p(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return isspace(escm_char_val(c)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_upper_case_p(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return isupper(escm_char_val(c)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_lower_case_p(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return islower(escm_char_val(c)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_char_to_integer(escm *e, escm_atom *args)
{
    escm_atom *c;
    escm_number *n;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    n = xmalloc(sizeof *n);
    n->fixnum = 1, n->d.ival = escm_char_val(c);

    return escm_atom_new(e, ESCM_TYPE_NUMBER, n);
}

escm_atom *
escm_integer_to_char(escm *e, escm_atom *args)
{
    escm_atom *n;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_NUMBER_ISINT(n), n, e);

    return escm_atom_new(e, ESCM_TYPE_CHAR,
			 (void *) (escm_intptr) escm_number_ival(n));
}

escm_atom *
escm_char_upcase(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return escm_atom_new(e, ESCM_TYPE_CHAR,
			 (void *) (escm_intptr) toupper(escm_char_val(c)));
}

escm_atom *
escm_char_downcase(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return escm_atom_new(e, ESCM_TYPE_CHAR,
			 (void *) (escm_intptr) tolower(escm_char_val(c)));
}

static void
char_print(escm *e, unsigned char c, FILE *stream)
{
    (void) e;

    fprintf(stream, "#\\");
    if (c == '\n')
	fprintf(stream, "newline");
    else if (c == ' ')
	fprintf(stream, "space");
    else
	fprintf(stream, "%lc", c);
}

static int
char_equal(escm *e, unsigned char c1, unsigned char c2, unsigned int lvl)
{
    (void) e;
    (void) lvl;

    return c1 == c2;
}

static int
char_parsetest(escm *e, int c)
{
    int c2, ret;

    if (c == '#') {
	c2 = escm_input_getc(e->input);
	ret = (c2 == '\\');
	escm_input_ungetc(e->input, c2);
	return ret;
    }

    return 0;
}    

static escm_atom *
char_parse(escm *e)
{
    unsigned char c;

    (void) escm_input_getc(e->input), escm_input_getc(e->input); /* skip #\ */
    c = escm_input_getchar(e->input);
    if (c == '\0')
	return NULL;

    return escm_char_make(e, c);
}
