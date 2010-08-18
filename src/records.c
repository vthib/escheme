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
#include <stdlib.h>
#include <string.h>

#include "escheme.h"

static escm_atom *make_record(escm *, escm_atom *, size_t);
static escm_atom *record_p(escm *, escm_atom *, size_t);
static escm_atom *get_record_member(escm *, escm_atom *, size_t);
static escm_atom *set_record_member(escm *, escm_atom *, size_t);
static size_t set_record(escm *, escm_atom **, escm_atom **, size_t);

static void record_mark(escm *, escm_atom **);
static void record_print(escm *, escm_atom **, escm_output *, int);
static int record_typecheck(escm *, escm_atom *, size_t);

void
escm_records_init(escm *e)
{
    escm_atom *a;

    a = escm_procedure_new(e, T("define-record"), 3, -1, escm_define_record,
                           NULL);
    escm_proc_val(a)->d.c.quoted = 0xF;
}

escm_atom *
escm_define_record(escm *e, escm_atom *args, void *nil)
{
    escm_type *t;
    escm_atom *herit, *memb, *a;
    unsigned long type;
    size_t ownsize, size, bufsize;
    tchar *buf, *name;

    (void) nil;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(a), a, e);
    name = escm_sym_name(a);

    herit = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSYM(herit) || herit == e->FALSE, herit, e);

    memb = args;
    ownsize = 0;
    while (args != e->NIL) {
        a = escm_cons_pop(e, &args);
        escm_assert(ESCM_ISSYM(a), a, e);
        ownsize++;
    }
    size = ownsize;

    if (herit != e->FALSE) {
        unsigned long t2;

        /* we run through the list backwards: record & dyn types are defined
           after the built-in, so if we encounter a built-in type, there is an
           error */
        for (type = e->ntypes - 1;; type--) {
            switch (e->types[type]->dtype) {
            case TYPE_BUILT:
                escm_error(e, _(T("~s: ~s is not a defined record.~%")),
                           escm_fun(e), herit);
                escm_abort(e);
            case TYPE_REC:
                if (0 == tcscmp(escm_sym_name(herit),
                                e->types[type]->d.rec.name))
                    goto out;
            default:
                break;
            }
        }
    out:
        for (t2 = type; t2 != 0; t2 = e->types[t2]->d.rec.parenttype)
            size += e->types[t2]->d.rec.len;
    }

    /* create the according type */
    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) record_mark;
    t->ffree = (Escm_Fun_Free) free;
    t->print.fprint = (Escm_Fun_Print) record_print;
    t->dtype = TYPE_REC;
    t->d.rec.members = memb;
    t->d.rec.len = size;
    t->d.rec.name = tcsdup(name);
    t->d.rec.parenttype = (herit != e->FALSE) ? type : 0;
    escm_type_add(e, t);

    /* now create the procedures make-name, name?, name:member,
       set-name:member! */

    /* + 30 for the moment. If short, we will realloc it */
    bufsize = tcslen(name) + 30;
    buf = xmalloc(sizeof *buf * bufsize);

    sntprintf(buf, bufsize, T("make-%") TFMT T("s"), name);
    (void) escm_procedure_new(e, buf, size, size,
                              (Escm_Fun_Prim) make_record,
                              (void *) (e->ntypes - 1));
    sntprintf(buf, bufsize, T("%") TFMT T("s?"), name);
    (void) escm_procedure_new(e, buf, 1, 1, (Escm_Fun_Prim) record_p,
                              (void *) (e->ntypes - 1));

    ownsize = size - ownsize;
    while (memb != e->NIL) {
        a = escm_cons_pop(e, &memb);
        if (sntprintf(buf, bufsize, T("set-%") TFMT T("s:%") TFMT T("s!"),
                     name, escm_sym_name(a)) >= (int) bufsize) {
            bufsize += tcslen(escm_sym_name(a));
            buf = xrealloc(buf, sizeof *buf * bufsize);
            sntprintf(buf, bufsize, T("set-%") TFMT T("s:%") TFMT T("s!"),
                     name, escm_sym_name(a));
        }
        (void) escm_procedure_new(e, buf, 2, 2,
                                  (Escm_Fun_Prim) set_record_member,
                                  (void *) (((e->ntypes - 1) << 8) | ownsize));
        sntprintf(buf, bufsize, T("%") TFMT T("s:%") TFMT T("s"), name,
                 escm_sym_name(a));
        (void) escm_procedure_new(e, buf, 1, 1,
                                  (Escm_Fun_Prim) get_record_member,
                                  (void *) (((e->ntypes - 1) << 8) | ownsize));
        ownsize++;
    }

    free(buf);

    return NULL;
}

static escm_atom *
make_record(escm *e, escm_atom *args, size_t type)
{
    escm_atom **rec;

    rec = xmalloc(sizeof *rec * e->types[type]->d.rec.len);
    (void) set_record(e, rec, &args, type);

    return escm_atom_new(e, type, rec);
}

static escm_atom *
record_p(escm *e, escm_atom *args, size_t type)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);

    return e->types[a->type]->dtype == TYPE_REC &&
        record_typecheck(e, a, type) ? e->TRUE : e->FALSE;
}

static escm_atom *
get_record_member(escm *e, escm_atom *args, size_t data)
{
    escm_atom *rec;
    size_t type, offset;

    rec = escm_cons_pop(e, &args);
    type = data >> 8;
    offset = data & 0xFF;

    escm_assert(e->types[rec->type]->dtype == TYPE_REC, rec, e);
    escm_assert(record_typecheck(e, rec, type), rec, e);

    return escm_rec_val(rec)[offset];
}

static escm_atom *
set_record_member(escm *e, escm_atom *args, size_t data)
{
    escm_atom *rec;
    size_t type, offset;

    rec = escm_cons_pop(e, &args);
    type = data >> 8;
    offset = data & 0xFF;

    escm_assert(e->types[rec->type]->dtype == TYPE_REC, rec, e);
    escm_assert(record_typecheck(e, rec, type), rec, e);

    escm_rec_val(rec)[offset] = escm_cons_pop(e, &args);
    return NULL;
}

static size_t
set_record(escm *e, escm_atom **rec, escm_atom **args, size_t type)
{
    size_t offset, i;

    offset = 0;
    if (e->types[type]->d.rec.parenttype != 0)
        offset = set_record(e, rec, args, e->types[type]->d.rec.parenttype);

    for (i = offset; i < e->types[type]->d.rec.len; i++)
        rec[offset++] = escm_cons_pop(e, args);

    return offset;
}

static void
record_mark(escm *e, escm_atom **rec)
{
    size_t i;

    for (i = 0; i < e->types[e->curobj->type]->d.rec.len; i++)
        escm_atom_mark(e, rec[i]);
}

static void
record_print(escm *e, escm_atom **rec, escm_output *stream, int lvl)
{
    size_t i, max;

    (void) rec;
    (void) lvl;

    escm_printf(stream, T("#<record %s {"),
                e->types[e->curobj->type]->d.rec.name);
    max = e->types[e->curobj->type]->d.rec.len;
    for (i = 0; i < max; i++) {
        escm_atom_print4(e, rec[i], stream, lvl);
        if (i < max - 1)
            escm_putc(stream, T(' '));
    }
    escm_printf(stream, T("} >"));
}

static int
record_typecheck(escm *e, escm_atom *a, size_t type)
{
    unsigned long t;

    for (t = a->type; t != 0; t = e->types[t]->d.rec.parenttype)
            if (t == type)
                return 1;

    return 0;
}
