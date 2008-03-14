project(Escheme)

set (FILES src/atom.c src/cons.c src/environments.c src/escm.c src/input.c
  src/main.c src/output.c src/primitives.c src/procedures.c src/srfi.c
  src/symbols.c src/tst.c src/utils.c)
set (DEFINES -Dinline="__inline__")

macro(append_dep)
  set(FILES ${FILES} ${ARGN})
endmacro(append_dep)

macro(append_define)
  set(DEFINES ${DEFINES} ${ARGN})
endmacro(append_define)

add_definitions(-g)
add_definitions($ENV{CFLAGS})

option(unicode "build the unicode version of strings and characters" 0)
option(c99 "use the c99 standard" 0)
option(r5rs "build a r5rs compliant version (override a lot of other options)"
       1)
option(unicode "build the unicode version of strings and characters" 0)
option(math "build math functions (sqrt, log, ...)" 1)

option(continuations "build the (partial) implementation of continuations" 0)
option(bnumbers "build the basic implementation of numbers" 1)
option(cnumbers "build the complete implementation of numbers" 1)
option(booleans "build the boolean implementation" 1)
option(strings "build the string implementation" 1)
option(characters "build the character implementation" 1)
option(ports "build the port implementation" 1)
option(vectors "build the vector implementation" 1)
option(macros "build the macro implementation" 1)
option(promises "build the promise implementation" 1)
option(dyntypes "build support for dynamic types" 1)

if (unicode)
  set(c99 1)
  append_define(-DESCM_USE_UNICODE)
  if (strings)
    append_dep(src/ustrings.c)
  endif(strings)
  if (characters)
    append_dep(src/uchars.c)
  endif(characters)
endif(unicode)

if (c99)
  append_define(-DESCM_USE_C99 -D_ISOC99_SOURCE)
endif(c99)

if (r5rs)
  set(cnumbers 1)
  set(booleans 1)
  set(characters 1)
  set(vectors 1)
  set(strings 1)
  set(promises 1)
  set(macros 1)
  set(ports 1)
  set(math 1)
endif (r5rs)

if (continuations)
  append_define(-DESCM_USE_CONTINUATIONS)
  append_dep(src/continuations.c)
endif(continuations)

if (bnumbers)
  append_define(-DESCM_USE_BNUMBERS)
  append_dep(src/bnumbers.c)
endif(bnumbers)

if (cnumbers)
  append_define(-DESCM_USE_CNUMBERS)
  append_dep(src/cnumbers.c)
endif(cnumbers)

if (booleans)
  append_define(-DESCM_USE_BOOLEANS)
  append_dep(src/booleans.c)
endif(booleans)

if (strings)
  append_define(-DESCM_USE_STRINGS)
  append_dep(src/astrings.c)
endif(strings)

if (characters)
  append_define(-DESCM_USE_CHARACTERS)
  append_dep(src/achars.c)
endif(characters)

if (ports)
  append_define(-DESCM_USE_PORTS)
  append_dep(src/ports.c)
endif(ports)

if (vectors)
  append_define(-DESCM_USE_VECTORS)
  append_dep(src/vectors.c)
endif(vectors)

if (macros)
  append_define(-DESCM_USE_MACROS)
  append_dep(src/macros.c)
endif(macros)

if (promises)
  append_define(-DESCM_USE_PROMISES)
  append_dep(src/promises.c)
endif(promises)

if (dyntypes)
  append_define(-DESCM_USE_DYNTYPES)
  append_dep(src/dyntypes.c)
endif(dyntypes)

add_executable(escheme ${FILES})

if (math)
  append_define(-DESCM_USE_MATH)
  find_library(libmath m)
  if (NOT libmath)
    message("Math library not found, disabling math support.")
  else (NOT libmath)
    target_link_libraries(escheme m)
  endif (NOT libmath)
endif (math)

add_definitions(${DEFINES})

