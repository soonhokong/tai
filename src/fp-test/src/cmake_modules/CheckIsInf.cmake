# Tries and find which isinf to use
# defines ISINF_VARIATION, which should be included in a configuration files somewher as
#   @ISINF_VARIATION@
if(ISINF_VARIATION)
  return()
endif(ISINF_VARIATION)

include(CheckCXXSourceCompiles)
CHECK_CXX_SOURCE_COMPILES(
  "#include <cmath>\nint main() { bool a = std::isinf(0e0); return 0; }\n"
  CXX_HAS_STD_ISINF)

if(NOT CXX_HAS_STD_ISINF)
  CHECK_CXX_SOURCE_COMPILES(
    "#include <math.h>\nint main() { bool a = isinf(0e0); return 0; }\n"
    CXX_HAS_ISINF)
endif(NOT CXX_HAS_STD_ISINF)

if(NOT CXX_HAS_STD_ISINF AND NOT CXX_HAS_ISINF)
  CHECK_CXX_SOURCE_COMPILES(
    "#include <math.h>\nint main() { bool a = _isinf(0e0); return 0; }\n"
    CXX_HAS___ISINF)
endif(NOT CXX_HAS_STD_ISINF AND NOT CXX_HAS_ISINF)

if(NOT CXX_HAS_STD_ISINF AND NOT CXX_HAS_ISINF)
  CHECK_CXX_SOURCE_COMPILES(
    "# include <float.h>\nint main() { bool a = _isinf(0e0); return 0; }\n"
    CXX_HAS_FLOAT_H_ISINF)
endif(NOT CXX_HAS_STD_ISINF AND NOT CXX_HAS_ISINF)

if(NOT CXX_HAS_STD_ISINF AND NOT CXX_HAS_ISINF AND NOT CXX_HAS___ISINF AND NOT CXX_HAS_FLOAT_H_ISINF)
  message(FATAL_ERROR "[isinf] could not find standard function on this OS.")
endif(NOT CXX_HAS_STD_ISINF AND NOT CXX_HAS_ISINF AND NOT CXX_HAS___ISINF AND NOT CXX_HAS_FLOAT_H_ISINF)

if(CXX_HAS_STD_ISINF)
  set(ISINF_HEADERS "#include <cmath>")
  set(ISINF_VARIATION "std::isinf")
elseif(CXX_HAS_ISINF)
  set(ISINF_HEADERS "#include <math.h>")
  set(ISINF_VARIATION "::isinf")
elseif(CXX_HAS___ISINF)
  set(ISINF_HEADERS "#include <math.h>")
  set(ISINF_VARIATION "__isinf")
elseif(CXX_HAS_FLOAT_H_ISINF)
  set(ISINF_HEADERS "#include <float.h>")
  set(ISINF_VARIATION "_isinf")
else()
  message(FATAL_ERROR "AM HERE")
endif()
if(ISINF_VARIATION)
  set(ISINF_VARIATION ${ISINF_VARIATION} CACHE INTERNAL "Definition for isinf\n")
  set(ISINF_HEADERS ${ISINF_HEADERS} CACHE INTERNAL "Headers containing isinf definition\n")
endif(ISINF_VARIATION)