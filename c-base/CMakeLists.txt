cmake_minimum_required(VERSION 3.9)
project(c_base C)

set(CMAKE_C_STANDARD 11)

add_library(c_base SHARED src/main/c/parameters.h src/main/c/server.h src/main/c/builders.h src/main/c/builders.c src/main/c/echo.c)