#pragma once

namespace Novo {

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

#define I64_MAX (9223372036854775807L)
#define I32_MAX (2147483647)
#define I16_MAX (32767)
#define I8_MAX  (127)

#define I64_MIN (-9223372036854775808UL)
#define I32_MIN (-2147483648)
#define I16_MIN (-32768)
#define I8_MIN  (-128)

#define GIBIBYTE(x) (x * 1024 * 1024 * 1024)
#define MEBIBYTE(x) (x * 1024 * 1024)
#define KIBIBYTE(x) (x * 1024)

#define GIGABYTE(x) (x * 1000 * 1000 * 1000)
#define MEGABYTE(x) (x * 1000 * 1000)
#define KILOBYTE(x) (x * 1000)

}
