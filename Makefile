.SUFFIXES:

#CC := gcc
CC := clang

CFLAGS := -g -pipe -O2 -Wall \
	-I. `sdl2-config --cflags` -fPIC \
	`python-config --cflags`

TEST_BIN := _fiend.so
TEST_LIBS := `sdl2-config --libs` -lpthread -shared

TEST_OBJ = \
	sdl.o \
	rng.o \
	fiend_wrap.o

ALL_BIN := $(TEST_BIN)
ALL_OBJ := $(TEST_OBJ)
ALL_DEP := $(patsubst %.o, .%.d, $(ALL_OBJ))
ALL_TARGETS := $(ALL_BIN)

TARGET: all

.PHONY: all clean check-syntax

all: $(ALL_BIN)

ifeq ($(filter clean, $(MAKECMDGOALS)),clean)
CLEAN_DEP := clean
else
CLEAN_DEP :=
endif

fiend_wrap.c: fiend.i fiendish.h
	@swig -python -builtin -I `sdl2-config --cflags` fiend.i

%.o %.d: %.c $(CLEAN_DEP) $(CONFIG_MAK) Makefile
	@echo " [C] $<"
	@$(CC) $(CFLAGS) -MMD -MF $(patsubst %.o, .%.d, $@) \
		-MT $(patsubst .%.d, %.o, $@) \
		-c -o $(patsubst .%.d, %.o, $@) $<

$(TEST_BIN): $(TEST_OBJ)
	@echo " [LINK] $@"
	@$(CC) $(CFLAGS) -o $@ $(TEST_OBJ) $(TEST_LIBS)

clean:
	rm -f $(ALL_TARGETS) $(ALL_OBJ) $(ALL_DEP)

# for flymake
check-syntax:
	$(CC) $(CFLAGS) -Wall -Wextra -pedantic -fsyntax-only $(patsubst %.o, %.c, $(ALL_OBJ))

ifneq ($(MAKECMDGOALS),clean)
-include $(ALL_DEP)
endif
