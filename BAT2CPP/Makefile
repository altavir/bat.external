.PHONY: all clean
JL_SHARE =/home/alexey/opt/share/julia

CC       = gcc
CFLAGS   = -Wall -g $(shell ${JL_SHARE}/julia-config.jl  --cflags)
CXX      = g++
CXXFLAGS = -Wall -g $(shell ${JL_SHARE}/julia-config.jl  --cflags)
LDFLAGS  = $(shell ${JL_SHARE}/julia-config.jl --ldflags --ldlibs)

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

${OBJS} : BAT2.hpp
