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

#include "env.h"
#include "escm.h"
#include "atom.h"
#include "utils.h"

#include "type/env.h"
#include "type/procedures.h"
#include "type/cons.h"
#include "type/symbols.h"

void
escm_addprims_env(escm *e)
{
    escm_atom *o;

    (void) escm_procedure_new(e, T("eval"), 1, 2, escm_prim_eval, NULL);

    (void) escm_procedure_new(e, T("scheme-report-environment"), 0, 1,
                              escm_scheme_report_environment, NULL);
    (void) escm_procedure_new(e, T("interaction-environment"), 0, 0,
                              escm_interaction_environment, NULL);

    o = escm_procedure_new(e, T("alpha"), 0, -1, escm_alpha, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, T("with"), 2, -1, escm_with, NULL);
    escm_proc_val(o)->d.c.quoted = 0x6;

    o = escm_procedure_new(e, T("library"), 2, -1, escm_library, NULL);
    escm_proc_val(o)->d.c.quoted = 0x7;
    o = escm_procedure_new(e, T("import"), 0, -1, escm_import, NULL);
    escm_proc_val(o)->d.c.quoted = 0x1;

    o = escm_procedure_new(e, T("begin"), 1, -1, escm_prim_begin, NULL);
    escm_proc_val(o)->d.c.quoted = 0x3;
}

escm_atom *
escm_prim_eval(escm *e, escm_atom *args, void *nil)
{
    escm_atom *expr, *env, *prev;

    (void) nil;
    expr = escm_cons_pop(e, &args);
    env = escm_cons_pop(e, &args);
    if (env) {
        escm_assert(ESCM_ISENV(env), env, e);
        prev = e->env, e->env = env;
    }

    expr = escm_atom_eval(e, expr);
    if (env)
        e->env = prev;

    return expr;
}

escm_atom *
escm_prim_begin(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return escm_begin(e, args, 1);
}

escm_atom *
escm_library(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a, *export, *name, *env, *prevenv;

    (void) nil;
    export = e->NIL;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(a) && a != e->NIL, a, e);

    name = escm_cons_car(a);
    escm_assert(ESCM_ISSYM(name), name, e);

    a = escm_cons_car(args);
    if (ESCM_ISCONS(a) && a != e->NIL && ESCM_ISSYM(escm_cons_car(a)) &&
        tcscmp(escm_sym_name(escm_cons_car(a)), T("export")) == 0) {
        export = escm_cons_pop(e, &args);
        export = escm_cons_cdr(export); /* skip 'export' symbol */
        a = escm_cons_car(args);
    }

    prevenv = e->env;

    if (ESCM_ISCONS(a) && a != e->NIL && ESCM_ISSYM(escm_cons_car(a)) &&
        tcscmp(escm_sym_name(escm_cons_car(a)), T("import")) == 0) {
        (void) escm_cons_pop(e, &a);
        escm_import(e, a, NULL);
    }

    (void) escm_env_enter(e, escm_env_new(e, e->env));
    escm_begin(e, args, 0);

    if (escm_env_val(e->env)->list == NULL)
        e->env = escm_env_val(e->env)->prev;

    env = escm_env_new(e, e->env);

    while ((a = escm_cons_pop(e, &export)) != NULL) {
        escm_assert1(ESCM_ISSYM(a), a, e, escm_env_leave(e, prevenv));
        escm_env_set(e, env, a, escm_sym_val(a));
    }

    escm_env_leave(e, prevenv);
    escm_env_set(e, e->env, name, env);

    return NULL;
}

escm_atom *
escm_import(escm *e, escm_atom *args, void *nil)
{
    escm_atom *env, *cons;

    (void) nil;
    while ((cons = escm_cons_pop(e, &args)) != NULL) {
        escm_assert(ESCM_ISCONS(cons), cons, e);
        env = escm_atom_eval(e, escm_cons_car(cons));
        if (!env) {
            escm_error(e, _(T("~s: ~s does not return an environment.~%")),
                       escm_fun(e), escm_cons_car(cons));
            escm_abort(e);
        } else if (e->err == 1)
            escm_abort(e);
        escm_assert(ESCM_ISENV(env), env, e);

        escm_env_enter(e, env);
    }

    return NULL;
}

escm_atom *
escm_alpha(escm *e, escm_atom *args, void *nil)
{
    escm_atom *env, *prev;

    (void) nil;
    env = escm_env_new(e, e->env);
    prev = escm_env_enter(e, env);

    (void) escm_begin(e, args, 0);
    if (e->err == 1) {
        escm_env_leave(e, prev);
        return NULL;
    }

    escm_env_leave(e, prev);
    return env;
}

escm_atom *
escm_with(escm *e, escm_atom *args, void *nil)
{
    escm_atom *env, *prev, *ret;

    (void) nil;
    env = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISENV(env), env, e);

    escm_env_val(env)->prev = e->env;
    prev = escm_env_enter(e, env);

    ret = escm_begin(e, args, 0);

    escm_env_leave(e, prev);
    return ret;
}

/* XXX: Write this function correctly */
escm_atom *
escm_scheme_report_environment(escm *e, escm_atom *args, void *nil)
{
    (void) args; (void) nil;

    return e->env;
}

escm_atom *
escm_interaction_environment(escm *e, escm_atom *args, void *nil)
{
    (void) args; (void) nil;

    return e->env;
}

escm_atom *
escm_begin(escm *e, escm_atom *args, int tailrec)
{
    escm_atom *a, *ret;

    ret = NULL;
    for (a = escm_cons_pop(e, &args); a; a = escm_cons_pop(e, &args)) {
        if (args == e->NIL && tailrec) {
            e->ctx->tailrec = 1;
            if (!escm_tailrec(e, a))
                return NULL;
        }
        ret = escm_atom_eval(e, a);
        if (!ret && e->err == 1)
            return ret;
    }

    return ret;
}
