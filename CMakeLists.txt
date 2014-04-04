cmake_minimum_required(VERSION 2.4.6)

include(CheckFunctionExists)

project(escheme C)

enable_testing()
set(TDIR ${CMAKE_SOURCE_DIR}/test)
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND})
add_test(test_symbols escheme ${TDIR}/test_symbols.scm)
add_test(test_lists escheme ${TDIR}/test_lists.scm)
add_test(test_srfi-2 escheme ${TDIR}/test_srfi-2.scm)

set (FILES src/core/atom.c src/core/escm.c src/core/input.c src/core/output.c
  src/core/scmpf.c src/core/tst.c src/core/utils.c)
set (FILES ${FILES} src/core/type/cons.c src/core/type/env.c
  src/core/type/procedures.c src/core/type/symbols.c)
set (FILES ${FILES} src/core/proc/cons.c src/core/proc/env.c
  src/core/proc/procedures.c src/core/proc/symbols.c)
set (FILES ${FILES} src/primitives.c src/srfi.c)
set (HEADER src/core/atom.h src/core/escm.h src/core/input.h src/core/output.h
  src/core/scmpf.h src/core/tst.h src/core/utils.h)
set (HEADER ${HEADER} src/core/type/cons.h src/core/type/env.h
  src/core/type/procedures.h src/core/type/symbols.h)
set (HEADER ${HEADER} src/core/proc/cons.h src/core/proc/env.h
  src/core/proc/procedures.h src/core/proc/symbols.h)
set (HEADER ${HEADER} src/primitives.h src/srfi.h)

set (DEFINES -Dinline="__inline__" -Wall -W -Wmissing-prototypes
  -pedantic-errors -g -coverage -O0 -std=c99
  -I. -I${CMAKE_SOURCE_DIR}/src/core)
#set (DEFINES -I.. -I. -Wall -W -pedantic)
#set (CMAKE_C_COMPILER "clang")

macro(append_dep)
  set(FILES ${FILES} ${ARGN})
endmacro(append_dep)

macro(append_header)
  set(HEADER ${HEADER} ${ARGN})
endmacro(append_header)

macro(append_define)
  set(DEFINES ${DEFINES} ${ARGN})
endmacro(append_define)

append_define(-g)
append_define($ENV{CFLAGS})

set(CMAKE_C_FLAGS "-W -Wall -pedantic -coverage")

check_function_exists(_vsnwprintf HAVE_VSNWPRINTF)

option(unicode "build the unicode version of strings and characters" 0)

option(continuations "build the (partial) implementation of continuations" 0)
option(numbers "build the basic implementation of numbers" 1)
option(booleans "build the boolean implementation" 1)
option(strings "build the string implementation" 1)
option(characters "build the character implementation" 1)
option(ports "build the port implementation" 1)
option(vectors "build the vector implementation" 1)
option(macros "build the macro implementation" 1)
option(promises "build the promise implementation" 1)
option(dyntypes "build support for dynamic types" 1)
option(records "build support for record types" 1)

if (unicode)
  append_define(-DESCM_UNICODE)
endif(unicode)

if (continuations)
  append_define(-DESCM_USE_CONTINUATIONS)
  append_dep(src/type/continuations.c)
  append_header(src/type/continuations.h)
endif(continuations)

if (numbers)
  append_define(-DESCM_USE_NUMBERS)
  append_dep(src/type/numbers.c)
  append_header(src/type/numbers.h)
  add_test(test_numbers escheme ${TDIR}/test_numbers.scm)
endif(numbers)

if (booleans)
  append_define(-DESCM_USE_BOOLEANS)
  append_dep(src/type/booleans.c)
  append_header(src/type/booleans.h)
  add_test(test_booleans escheme ${TDIR}/test_booleans.scm)
endif(booleans)

if (strings)
  append_define(-DESCM_USE_STRINGS)
  append_dep(src/type/strings.c)
  append_header(src/type/strings.h)
  add_test(test_strings escheme ${TDIR}/test_strings.scm)
endif(strings)

if (characters)
  append_define(-DESCM_USE_CHARACTERS)
  append_dep(src/type/chars.c)
  append_header(src/type/chars.h)
  add_test(test_characters escheme ${TDIR}/test_characters.scm)
endif(characters)

if (ports)
  append_define(-DESCM_USE_PORTS)
  append_dep(src/type/ports.c)
  append_header(src/type/ports.h)
endif(ports)

if (vectors)
  append_define(-DESCM_USE_VECTORS)
  append_dep(src/type/vectors.c)
  append_header(src/type/vectors.h)
  add_test(test_vectors escheme ${TDIR}/test_vectors.scm)
endif(vectors)

if (macros)
  append_define(-DESCM_USE_MACROS)
  append_dep(src/type/macros.c)
  append_header(src/type/macros.h)
  add_test(test_macros escheme ${TDIR}/test_macros.scm)
endif(macros)

if (records)
  append_define(-DESCM_USE_RECORDS)
  append_dep(src/type/records.c)
  append_header(src/type/records.h)
  add_test(test_records escheme ${TDIR}/test_records.scm)
endif(records)

if (promises)
  append_define(-DESCM_USE_PROMISES)
  append_dep(src/type/promises.c)
  append_header(src/type/promises.h)
  add_test(test_promises escheme ${TDIR}/test_promises.scm)
endif(promises)

if (dyntypes)
  append_define(-DESCM_USE_DYNTYPES)
  append_dep(src/type/dyntypes.c)
  append_header(src/type/dyntypes.h)
endif(dyntypes)


add_library(escm SHARED ${FILES} ${HEADER})
add_executable(escheme src/main.c)
target_link_libraries(escheme escm)

add_definitions(${DEFINES})

find_library(libmath m)
if (NOT libmath)
  error(STATUS "Math library not found.")
else (NOT libmath)
  target_link_libraries(escm m)
endif (NOT libmath)

target_link_libraries(escm gcov)

set(ESCM_SHARE_PATH "${CMAKE_INSTALL_PREFIX}/share/escheme/")
#set(ESCM_SHARE_PATH "./")

configure_file(config.h.cmake config.h)

install(FILES init.scm DESTINATION ${ESCM_SHARE_PATH})
install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/escheme DESTINATION bin)
install(TARGETS escm DESTINATION lib)
install(FILES ${HEADER} DESTINATION include)
