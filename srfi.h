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
#ifndef ESCHEME_SRFI_H
# define ESCHEME_SRFI_H

void escm_srfi_init(escm *);

/* srfi 6 */
escm_atom *escm_srfi_open_input_string(escm *, escm_atom *);

/* srfi 23 */
escm_atom *escm_srfi_error(escm *, escm_atom *);

#endif /* ESCHEME_SRFI_H */
