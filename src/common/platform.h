#pragma once

#include "defines.h"
#include "nstring.h"

namespace Novo {

struct Allocator;

struct File_Handle
{
    void *handle;
    bool valid = false;
};

enum File_Mode : u32
{
    FILE_MODE_READ  = 0x01,
    FILE_MODE_WRITE = 0x02,
};

NAPI bool fs_read_entire_file(Allocator *allocator, const String_Ref path, String *out_string);

NAPI bool fs_open(const String_Ref path, File_Mode mode, File_Handle *out_handle);
NAPI bool fs_size(File_Handle *handle, u64 *out_size);
NAPI bool fs_read(File_Handle *handle, u64 size, u8 *out_bytes, u64 *out_size);

NAPI void fs_chdir(const String_Ref path);
NAPI bool fs_is_realpath(const String_Ref path);
NAPI String fs_realpath(Allocator *allocator, const String_Ref path);
NAPI bool fs_is_directory(const String_Ref path);
NAPI bool fs_is_file(const String_Ref path);

}
