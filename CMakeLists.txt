cmake_minimum_required(VERSION 2.4.6)

include(CheckFunctionExists)

project(escheme C)

enable_testing()
add_test(test_symbols escheme test/test_symbols.scm)
add_test(test_lists escheme test/test_lists.scm)

set (FILES src/atom.c src/cons.c src/environments.c src/escm.c src/input.c
  src/output.c src/primitives.c src/procedures.c src/srfi.c src/symbols.c
  src/tst.c src/utils.c)
set (HEADER src/types.h src/escheme.h src/atom.h src/cons.h src/environments.h
  src/escm.h src/input.h src/output.h src/primitives.h src/procedures.h
  src/srfi.h src/symbols.h src/tst.h src/utils.h)
set (DEFINES -Dinline="__inline__" -I.. -I. -Wall -W -Wmissing-prototypes
  -pedantic-errors)
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

check_function_exists(_vsnwprintf HAVE_VSNWPRINTF)

option(unicode "build the unicode version of strings and characters" 0)
option(c99 "use the c99 standard" 0)
option(math "build math functions (sqrt, log, ...)" 1)

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
  if (NOT c99)
    message("Unicode support is enabled but c99 isn't. Forcing c99 support...")
  endif(NOT c99)
  set(c99 1)
  append_define(-DESCM_UNICODE)
endif(unicode)

if (c99)
  append_define(-DESCM_USE_C99 -D_ISOC99_SOURCE)
endif(c99)

if (continuations)
  append_define(-DESCM_USE_CONTINUATIONS)
  append_dep(src/continuations.c)
  append_header(src/continuations.h)
endif(continuations)

if (numbers)
  append_define(-DESCM_USE_NUMBERS)
  append_dep(src/numbers.c)
  append_header(src/numbers.h)
  add_test(test_numbers escheme test/test_numbers.scm)
endif(numbers)

if (booleans)
  append_define(-DESCM_USE_BOOLEANS)
  append_dep(src/booleans.c)
  append_header(src/booleans.h)
  add_test(test_booleans escheme test/test_booleans.scm)
endif(booleans)

if (strings)
  append_define(-DESCM_USE_STRINGS)
  append_dep(src/strings.c)
  append_header(src/strings.h)
  add_test(test_strings escheme test/test_strings.scm)
endif(strings)

if (characters)
  append_define(-DESCM_USE_CHARACTERS)
  append_dep(src/chars.c)
  append_header(src/chars.h)
  add_test(test_characters escheme test/test_characters.scm)
endif(characters)

if (ports)
  append_define(-DESCM_USE_PORTS)
  append_dep(src/ports.c)
  append_header(src/ports.h)
endif(ports)

if (vectors)
  append_define(-DESCM_USE_VECTORS)
  append_dep(src/vectors.c)
  append_header(src/vectors.h)
  add_test(test_vectors escheme test/test_vectors.scm)
endif(vectors)

if (macros)
  append_define(-DESCM_USE_MACROS)
  append_dep(src/macros.c)
  append_header(src/macros.h)
  add_test(test_macros escheme test/test_macros.scm)
endif(macros)

if (records)
  append_define(-DESCM_USE_RECORDS)
  append_dep(src/records.c)
  append_header(src/records.h)
#  add_test(test_records escheme test/test_records.scm)
endif(records)

if (promises)
  append_define(-DESCM_USE_PROMISES)
  append_dep(src/promises.c)
  append_header(src/promises.h)
  add_test(test_promises escheme test/test_promises.scm)
endif(promises)

if (dyntypes)
  append_define(-DESCM_USE_DYNTYPES)
  append_dep(src/dyntypes.c)
  append_header(src/dyntypes.h)
endif(dyntypes)

if (math)
  append_define(-DESCM_USE_MATH)
  find_library(libmath m)
endif (math)

add_library(escm SHARED ${FILES} ${HEADER})
add_executable(escheme src/main.c)
target_link_libraries(escheme escm)

add_definitions(${DEFINES})
if (NOT libmath)
  message(STATUS "Math library not found, disabling math support.")
else (NOT libmath)
  target_link_libraries(escm m)
endif (NOT libmath)

#set(ESCM_SHARE_PATH "${CMAKE_INSTALL_PREFIX}/share/escheme/")
set(ESCM_SHARE_PATH "./")

configure_file(config.h.cmake config.h)

install(FILES init.scm DESTINATION ${ESCM_SHARE_PATH})
install(PROGRAMS escheme DESTINATION bin)
install(TARGETS escm DESTINATION lib)
install(FILES ${HEADER} DESTINATION include)
