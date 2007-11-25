SONAME = escheme.so
EXEC = escheme
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
HEADERS = $(wildcard *.h)
DEP = $(SRC:.c=.d)

CC ?= gcc
CFLAGS ?= -g
FLAGS = -Wall -W -Wmissing-prototypes -std=c99 -pedantic-errors -fPIC $(CFLAGS)
#FLAGS = -Wall -restrict -fast $(CFLAGS)
CPPFLAGS = -DESCM_USE_UNICODE -D_ISOC99_SOURCE -I. -DESCM_R5RS \
	-Dinline="__inline__"
LDFLAGS = -lm
SOFLAGS = -shared

#all: $(SONAME)
all: $(EXEC)

.PHONY: clean mrproper

$(SONAME): $(OBJ)
	$(CC) $^ $(CFLAGS) $(SOFLAGS) -o $@

$(EXEC): $(OBJ)
	$(CC) $^ $(CFLAGS) $(LDFLAGS) -o $@

%.o: %.c %.d
	$(CC) $(FLAGS) $(CPPFLAGS) -c $< -o $@

%.d: %.c
	$(CC) $(FLAGS) $(CPPFLAGS) -MM -MF $@ $<

clean:
	for OBJ in $(OBJ) $(DEP); do \
		if [ -e $$OBJ ]; then \
			rm $$OBJ; \
		fi \
	done

mrproper: clean
	if [ -e $(EXEC) ]; then \
		rm $(EXEC); \
	fi

-include $(DEP)
