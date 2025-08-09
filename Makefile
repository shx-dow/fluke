CFLAGS := -std=c99 -Wall -Wextra -pedantic -O2
DEBUGFLAGS := -g -fsanitize=address,undefined

all: fluke

debug: CFLAGS += $(DEBUGFLAGS)
debug: clean fluke

fluke: Fluke.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f fluke a.out *.o