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
#include <locale.h>

#include "escheme.h"

int
main(void)
{
    escm *e;

    if (!setlocale(LC_ALL, "") ||
        !setlocale(LC_NUMERIC, "C"))  /* corrects strtod interpretation */
        fprintf(stderr, "can't set the locale.\n");

    e = escm_new();
    if (!e)
	return EXIT_FAILURE;

    escm_shell(e);
    escm_free(e);

    return EXIT_SUCCESS;
}
