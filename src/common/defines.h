#pragma once

#ifdef NEXPORT

// Exports
#ifdef _MSC_VER
#define NAPI __declspec(dllexport)
#else
#define NAPI __attribute__((visibility("default")))
#endif
#else
// Imports
#ifdef _MSC_VER
#define NAPI __declspec(dllimport)
#else
#define NAPI
#endif

#endif // NEXPORT

#if (defined(__clang__) || defined(__gcc__)) && (defined(__STDC_VERSION__)  && __STDC_VERSION__ > 201112L)
#define STATIC_ASSERT _Static_assert
#else
#define STATIC_ASSERT static_assert
#endif




#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)

#define NPLATFORM_WINDOWS 1
#ifndef _WIN64
#error "64-bit is required on Windows!"
#endif // _WIN64

#elif defined(__linux__) || defined(__gnu_linux__)

//STATIC_ASSERT(false, "Unsupported platform (linux).");
#define NPLATFORM_LINUX 1

#elif defined(__unix__)

#define NPLATFORM_UNIX 1
STATIC_ASSERT(false, "Unsupported platform (unix).");

#elif __APPLE__

#define NPLATFORM_APPLE 1
STATIC_ASSERT(false, "Unsupported platform (Apple).");

#endif // defined(WIN32) || defined(_WIN32) || defined(__WIN32__)



#if defined(__clang__) || defined(__gcc__)
#define NINLINE __attribute__((always_inline)) inline
#define NNOINLINE __attribute__((noinline))
#elif defined(_MSC_VER)
#define NINLINE __forceinline
#define NNOINLINE __declspec(noinline)
#else
#define NINLINE static inline
#define NNOINLINE
#endif

#define GIBIBYTE(x) (x##UL * 1024UL * 1024UL * 1024UL)
#define MEBIBYTE(x) (x##UL * 1024UL * 1024UL)
#define KIBIBYTE(x) (x##UL * 1024UL)

#define GIGABYTE(x) (x##UL * 1000UL * 1000UL * 1000UL)
#define MEGABYTE(x) (x##UL * 1000UL * 1000UL)
#define KILOBYTE(x) (x##UL * 1000UL)

namespace Novo {

typedef unsigned char       u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef signed char       s8;
typedef signed short     s16;
typedef signed int       s32;
typedef signed long long s64;

typedef float  r32;
typedef double r64;

typedef u64 p_uint_t;

STATIC_ASSERT(sizeof(u8) == 1, "Expected sizeof(u8) to be 1 byte");
STATIC_ASSERT(sizeof(u16) == 2, "Expected sizeof(u16) to be 2 bytes");
STATIC_ASSERT(sizeof(u32) == 4, "Expected sizeof(u32) to be 4 bytes");
STATIC_ASSERT(sizeof(u64) == 8, "Expected sizeof(u64) to be 8 bytes");

STATIC_ASSERT(sizeof(s8) == 1, "Expected sizeof(s8) to be 1 byte");
STATIC_ASSERT(sizeof(s16) == 2, "Expected sizeof(s16) to be 2 bytes");
STATIC_ASSERT(sizeof(s32) == 4, "Expected sizeof(s32) to be 4 bytes");
STATIC_ASSERT(sizeof(s64) == 8, "Expected sizeof(s64) to be 8 bytes");

STATIC_ASSERT(sizeof(r32) == 4, "Expected sizeof(r32) to be 4 bytes");
STATIC_ASSERT(sizeof(r64) == 8, "Expected sizeof(r64) to be 8 bytes");

#define U64_MAX (18446744073709551615UL)
#define U32_MAX (4294967295U)
#define U16_MAX (65535U)
#define U8_MAX  (255U)

#define I64_MAX (9223372036854775807LL)
#define I32_MAX (2147483647L)
#define I16_MAX (32767)
#define I8_MAX  (127)

#define I64_MIN (-9223372036854775807LL - 1L)
#define I32_MIN (-2147483647LL - 1L)
#define I16_MIN (-32768)
#define I8_MIN  (-128)

NINLINE u64 get_aligned(u64 operand, u64 alignment) {
    return ((operand + (alignment - 1)) & ~(alignment - 1));
}

struct Real_Value
{
    float r32;
    double r64;
};

template <typename T>
T max(T a, T b) {
    if (a > b) return a;
    return b;
}

template <typename T>
T min(T a, T b) {
    if (a < b) return a;
    return b;
}

}
