#ifndef ROBOTSTXTBING_EXPORT_H_
#define ROBOTSTXTBING_EXPORT_H_

#if defined(_WIN32) || defined(__CYGWIN__)
#  if defined(ROBOTSTXTBING_SHARED)
#    if defined(ROBOTSTXTBING_BUILDING_LIBRARY)
#      define ROBOTSTXTBING_EXPORT __declspec(dllexport)
#    else
#      define ROBOTSTXTBING_EXPORT __declspec(dllimport)
#    endif
#  else
#    define ROBOTSTXTBING_EXPORT
#  endif
#elif defined(ROBOTSTXTBING_SHARED) && (defined(__GNUC__) || defined(__clang__))
#  define ROBOTSTXTBING_EXPORT __attribute__((visibility("default")))
#else
#  define ROBOTSTXTBING_EXPORT
#endif

#endif  // ROBOTSTXTBING_EXPORT_H_
