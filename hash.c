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
#include <assert.h>

#include "hash.h"
#include "utils.h"

#define FNV_PRIME 16777619UL
#define FNV_OFFSET 2166136261UL

struct item {
    char *key;
    void *data;
    Escm_Fun_Free free_cb;
    struct item *link;
};

static unsigned long dohash(escm_hash *, const char *);
static unsigned long xorfold(unsigned long, unsigned);
static unsigned long hashstr(const void *, size_t);

/**
 * @brief return an new, initialized hashtable
 * @return the new hashtable or @c NULL if failed
 */
escm_hash *
escm_hash_new(unsigned long size)
{
    escm_hash *hash = xmalloc(sizeof *hash);

    for (hash->sizelog2 = 0; (1UL << hash->sizelog2) < size;
	 hash->sizelog2++)
	;
    if (hash->sizelog2 > 16) {
	hash->size = (1UL << 16);
	hash->sizelog2 = 16;
    } else
	hash->size = (1UL << hash->sizelog2);

    hash->map = xcalloc(1, sizeof *hash->map * hash->size);

    return hash;
}


/**
 * @brief clear and free a hash
 */
void
escm_hash_free(escm_hash *hash)
{
    struct item *p, *tmp;
    unsigned long n;

    assert(hash != NULL);

    for (n = 0; n < hash->size; n++) {
	p = hash->map[n];
	while (p) {
	    tmp = p->link;
	    if (p->free_cb)
		p->free_cb(p->data);
	    free(p->key);
	    free(p);
	    p = tmp;
	}
    }
    free(hash->map);

    free(hash);
}

/**
 * @brief insert a new data in the hash
 */
void
escm_hash_set(escm_hash *hash, const char *key, const void *data)
{
    escm_hash_setfull(hash, key, data, NULL);
}

/**
 * @brief insert a data and set the function used to free it
 */
void
escm_hash_setfull(escm_hash *hash, const char *key, const void *data,
		 Escm_Fun_Free free_cb)
{
    struct item *p;
    unsigned long k;

    assert(hash != NULL);
    assert(key != NULL);

    k = dohash(hash, key);

    for (p = hash->map[k]; p != NULL && strcmp(key, p->key) != 0; p = p->link)
	;

    if (!p) {
	p = xmalloc(sizeof *p);
	p->link = hash->map[k];
	hash->map[k] = p;
	p->key = xstrdup(key);
    }

    p->data = (void *) data;
    p->free_cb = free_cb;
}

/**
 * @brief delete a data of the hash
 */
void
escm_hash_del(escm_hash *hash, const char *key)
{
    struct item *p, **prev;
    unsigned long k;

    assert(hash != NULL);
    assert(key != NULL);

    k = dohash(hash, key);

    p = hash->map[k];

    prev = &hash->map[k];
    while (p != NULL && strcmp(key, p->key) != 0)
	prev = &p->link, p = p->link;

    if (p) {
	*prev = p->link;
	if (p->free_cb != NULL)
	    p->free_cb(p->data);
	free(p->key);
	free(p);
    }
}

/**
 * @brief return the data inteded by a word, or NULL if failed
 */
void *
escm_hash_get(escm_hash *hash, const char *key)
{
    struct item *p;
    unsigned long k;

    assert(hash != NULL);
    assert(key != NULL);

    k = dohash(hash, key);

    for (p = hash->map[k]; p != NULL && strcmp(key, p->key) != 0; p = p->link)
	;

    return (p) ? p->data : NULL;
}

/**
 * @brief applicate a function on each data
 * @warning This function works only if the hash contains only datas of the same
 * type
 */
void
escm_hash_foreach(escm_hash *hash, Escm_Fun_Foreach fun, void *data)
{
    struct item *p;
    unsigned long i;

    assert(hash != NULL);
    assert(fun != NULL);

    for (i = 0; i < hash->size; i++) {
	for (p = hash->map[i]; p; p = p->link)
	    fun(data, p->data);
    }
}

/*--- privates functions ---*/

static unsigned long
dohash(escm_hash *hash, const char *key)
{
    unsigned long k;

    assert(hash != NULL);
    assert(key != NULL);

    k = hashstr(key, strlen(key));
    return xorfold(k, hash->sizelog2) % hash->size;
}

static unsigned long
xorfold(unsigned long k, unsigned m)
{
    if (m < 16)
	return ((k >> m) ^ k) & ((1UL << m) - 1UL);
    else
	return (k >> 16) ^ (k & 0xffff);
}

static unsigned long
hashstr(const void *buf, size_t len)
{
    const unsigned char *bp, *be;
    unsigned long k = FNV_OFFSET;

    assert(buf != NULL);

    bp = buf;
    be = bp + len;

    while (bp < be) {
	k *= FNV_PRIME;
	k ^= *bp++;
    }

    return k;
}
