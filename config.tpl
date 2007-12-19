{ "unicode", "ESCM_USE_UNICODE", NULL, "build the unicode version of strings "
  "and characters", 0 },
{ "continuations", "ESCM_USE_CONTINUATIONS", " continuations.c",
  "build the (broken) continuation implementation", 0 },
{ "bnumbers", "ESCM_USE_BNUMBERS", " bnumbers.c",
  "build the basic impl of numbers", 1 },
{ "cnumbers", "ESCM_USE_CNUMBERS", " cnumbers.c",
  "build the complete impl of numbers", 1 },
{ "booleans", "ESCM_USE_BOOLEANS", " booleans.c",
  "build the boolean implementation", 1 },
{ "strings", "ESCM_USE_STRINGS", " astrings.c",
  "build the string implementation", 1 },
{ "characters", "ESCM_USE_CHARACTERS", " achars.c",
  "build the character implementation", 1 },
{ "ports", "ESCM_USE_PORTS", " ports.c", "build the port implementation", 1 },
{ "vectors", "ESCM_USE_VECTORS", " vectors.c", "build the vector implementation",
  1 },
{ "macros", "ESCM_USE_MACROS", " macros.c", "build the macro implementation",
  1 },
{ "promises", "ESCM_USE_PROMISES", " promises.c",
  "build the promise implementation", 1 },
{ "dyntype", "ESCM_USE_DYNTYPE", " type.c",
  "build support for dynamic types", 1 }
