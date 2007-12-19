CONFIGURE = configure
EXEC = escheme
SRCDIR = src

CC ?= gcc

.PHONY: clean mrproper

all:
	if [ ! -e $(CONFIGURE) ]; then \
		$(MAKE) config; \
	else \
		$(MAKE) $(EXEC); \
	fi

config:
	$(CC) config.c -o $(CONFIGURE)

$(EXEC):
	cd $(SRCDIR) && $(MAKE) $@
	cp $(SRCDIR)/$(EXEC) .

clean:
	cd $(SRCDIR) && $(MAKE) $@

mrproper:
	cd $(SRCDIR) && $(MAKE) $@
	if [ -e $(CONFIGURE) ]; then \
		rm $(CONFIGURE); \
	fi
	if [ -e $(EXEC) ]; then \
		rm $(EXEC); \
	fi
