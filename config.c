#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_BUF 255

/* basic tst impl to handle customs vars (e.g. CC, CFLAGS, etc) */
struct tst {
    struct tst *lo, *hi, *down;
    char cval;
    char *val;
};

struct elem {
    const char *name;
    const char *macro;
    const char *files;
    const char *help;
    int val;
};

enum {
    UNICODE, CONT, BNUMB, CNUMB, BOOL, STRING, CHAR, PORT, VECT, MACRO, PROMISE,
    DYNTYPE, MAX_ELEMS
};

static int write_config(struct elem *);
static int modify(struct tst *, const char *);

static char *tst_get(struct tst *, const char *);
static void tst_set(struct tst **, const char *, const char *);
static void tst_free(struct tst *);

static void *xmalloc(size_t);
static void *xcalloc(size_t, size_t);
static char *xstrdup(const char *);

int
main(int argc, char **argv)
{
    int i, j;
    char *bp, *ep;
    struct tst *tree = NULL;
    struct elem elems[MAX_ELEMS] = {
#include "config.tpl"
    };
    char srcs[255];

    snprintf(srcs, 500, "atom.c cons.c environments.c escm.c input.c main.c "
	     "output.c primitives.c procedures.c srfi.c symbols.c tst.c "
	     "utils.c");

    /* add default values */
    tst_set(&tree, "CC", "gcc");
    tst_set(&tree, "CFLAGS", "-g");

    for (i = 1; i < argc; i++) {
	if (0 == strcmp(argv[i], "--help") || 0 == strcmp(argv[i], "-h")) {
	    for (j = 0; j < MAX_ELEMS; j++)
		printf("--%s=yes|no\t%s (default is %s).\n", elems[j].name,
		       elems[j].help, (elems[j].val == 1) ? "yes" : "no");
	    return EXIT_SUCCESS;
	}

	bp = argv[i];
	while (*bp == '-')
	    bp++;

	ep = bp;
	while (*ep != '=' && *ep != '\0')
	    ep++;
	if (*ep != '\0')
	    *ep++ = '\0';

	for (j = 0; j < MAX_ELEMS; j++) {
	    if (0 == strcmp(bp, elems[j].name)) {
		elems[j].val = !(0 == strcmp(ep, "no"));
		break;
	    }
	}
	if (j == MAX_ELEMS)
	    tst_set(&tree, bp, ep);
    }

    /* add the files to compile */
    for (j = 0; j < MAX_ELEMS; j++) {
	if (elems[j].val) {
	    if (elems[j].files)
		strcat(srcs, elems[j].files);
	}
    }
    if (elems[UNICODE].val) {
	if (elems[STRING].val)
	    strcat(srcs, " ustrings.c");
	if (elems[CHAR].val)
	    strcat(srcs, " uchars.c");
    }

    tst_set(&tree, "SRCS", srcs);

    if (0 == write_config(elems))
	return EXIT_FAILURE;
    printf("config.h writen.\n");

    modify(tree, "src/Makefile.in");
    printf("src/Makefile created.\n");

    tst_free(tree);

    return EXIT_SUCCESS;
}

static int
write_config(struct elem *elems)
{
    FILE *out;
    int i;

    out = fopen("config.h", "w");
    if (!out) {
	fprintf(stderr, "can't open config.h for writing.\n");
	return 0;
    }

    /* XXX: write copyright */
    for (i = 0; i < MAX_ELEMS; i++) {
	if (elems[i].val)
	    fprintf(out, "#define %s\n", elems[i].macro);  
    }

    fclose(out);
    return 1;
}

static int
modify(struct tst *tree, const char *file)
{
    FILE *fout, *fin;
    char *out;
    size_t len;
    int c;

    /* XXX: find the correct name */
    len = strlen(file) - 2;
    out = xmalloc(sizeof *out * len);
    strncpy(out, file, len - 1);
    out[len - 1] = '\0';

    fout = fopen(out, "w");
    if (!fout) {
	fprintf(stderr, "can't open %s for writing.\n", out);
	goto err_fout;
    }
    fin = fopen(file, "r");
    if (!fout) {
	fprintf(stderr, "can't open %s for reading.\n", file);
	goto err_fin;
    }

    while ((c = fgetc(fin)) != EOF) {
	if (c == '@') {
	    int c2;

	    c2 = fgetc(fin);
	    if (c2 == '@')
		fputc('@', fout);
	    else {
		char buf[MAX_BUF];
		char *p;
		int i;

		buf[0] = c2;
		for (i = 1; (c2 = fgetc(fin)) != '@' && c2 != EOF; i++)
		    buf[i] = c2;
		buf[i] = '\0';

		p = tst_get(tree, buf);
		/* XXX: write conditional vars e.g. @CC:gcc@ */
		if (p)
		    fputs(p, fout);
		if (c2 == EOF)
		    break;
	    }
	} else
	    fputc(c, fout);
    }

    fclose(fin);
    fclose(fout);
    free(out);
    return 1;

err_fin:
    fclose(fout);
err_fout:
    free(out);
    return 0;
}


/* tst impl */
static char *
tst_get(struct tst *t, const char *s)
{
    if (!t)
        return NULL;

    if (*s < t->cval)
        return tst_get(t->lo, s);
    else if (*s > t->cval)
        return tst_get(t->hi, s);
    else {
        if (*(s + 1) == '\0')
            return t->val;
        else
	    return tst_get(t->down, s + 1);
    }
}

static void
tst_set(struct tst **t, const char *s, const char *val)
{
    if (*s == '\0')
        return;

    if (!*t) {
        *t = xcalloc(1, sizeof **t);
        (*t)->cval = *s;
    }

    if (*s < (*t)->cval)
        tst_set(&(*t)->lo, s, val);
    else if (*s > (*t)->cval)
        tst_set(&(*t)->hi, s, val);
    else {
        if (*(s + 1) == '\0')
	    (*t)->val = xstrdup(val);
	else
	    tst_set(&(*t)->down, s + 1, val);
    }
}

static void
tst_free(struct tst *t)
{
    if (!t)
        return;

    tst_free(t->lo);
    tst_free(t->down);
    tst_free(t->hi);

    free(t->val);
    free(t);
}

static void *
xmalloc(size_t n)
{
    void *p;

    p = malloc(n);
    if (!p) {
        fprintf(stderr, "Memory is too low\n");
        exit(EXIT_FAILURE);
    }
    return p;
}

static void *
xcalloc(size_t nelem, size_t n)
{
    void *p;

    p = calloc(nelem, n);
    if (!p) {
        fprintf(stderr, "Memory is too low\n");
        exit(EXIT_FAILURE);
    }
    return p;
}

static char *
xstrdup(const char *s)
{
    size_t len;
    char *copy;

    len = strlen(s) + 1;
    copy = xmalloc(sizeof(char) * len);
    memcpy(copy, s, len);

    return copy;
}

