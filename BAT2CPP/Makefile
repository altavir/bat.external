.PHONY: all clean
# Julia-related flags
JL_CFG      = $(shell dirname $$(which julia))/../share/julia/julia-config.jl
JL_CFLAGS   = $(shell ${JL_CFG} --cflags)
JL_CXXFLAGS = $(shell ${JL_CFG} --cflags | sed s/-std=gnu99//)
JL_LDFLAGS  = $(shell ${JL_CFG} --ldflags --ldlibs)
# Compiler flags
CC       = gcc
CFLAGS   = -Wall -g ${JL_CFLAGS}
CXX      = g++
CXXFLAGS = -Wall -g ${JL_CXXFLAGS}
LDFLAGS  = ${JL_LDFLAGS}

OBJS = main.o BAT2.o julia-cpp.o

all: test
clean:
	rm -rf *.o test

test: ${OBJS}
	${CXX} -o $@ ${CFLAGS} ${LDFLAGS} $+

%.o : %.c
	${CC} -c -o $@ ${CFLAGS} $<
%.o : %.cpp
	${CXX} -c -o $@ ${CXXFLAGS} $<

${OBJS} : BAT2.hpp julia-cpp.hpp
