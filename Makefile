.SUFFIXES:

CXX := g++

CXXFLAGS := -g -pipe -O2 -Wall \
	-I. `sdl2-config --cflags` \
	--std=c++11 -D_REENTRANT -DHAVE_OPENGL

TEST_BIN := fiendish
TEST_LIBS := `sdl2-config --libs` -lpthread -lGL
TEST_OBJ = \
	main.o \
	sdlman.o \
	colors.o \
	rng.o \
	keyboard.o \
	playstate.o \
	globals.o \
    dungeon.o \
	shadow.o \
	text.o \
	messages.o \
	message_state.o

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

%.o %.d: %.cpp $(CLEAN_DEP) $(CONFIG_MAK) Makefile
	@echo " [C] $<"
	@$(CXX) $(CXXFLAGS) -MMD -MF $(patsubst %.o, .%.d, $@) \
		-MT $(patsubst .%.d, %.o, $@) \
		-c -o $(patsubst .%.d, %.o, $@) $<

$(TEST_BIN): $(TEST_OBJ)
	@echo " [LINK] $@"
	@$(CXX) $(CXXFLAGS) -o $@ $(TEST_OBJ) $(TEST_LIBS)

clean:
	rm -f $(ALL_TARGETS) $(ALL_OBJ) $(ALL_DEP)

# for flymake
check-syntax:
	$(CXX) $(CXXFLAGS) -Wall -Wextra -pedantic -fsyntax-only $(patsubst %.o, %.cpp, $(ALL_OBJ))

ifneq ($(MAKECMDGOALS),clean)
-include $(ALL_DEP)
endif
