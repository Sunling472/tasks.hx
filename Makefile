CC      ?= gcc
CFLAGS  ?= -O2 -Wall -Wextra

bin/hx-task: hx-task.c
	@mkdir -p bin
	$(CC) $(CFLAGS) -o $@ $< -lutil

clean:
	rm -f bin/hx-task

.PHONY: clean
