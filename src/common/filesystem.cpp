#include "filesystem.h"

#include "logger.h"
#include "memory/allocator.h"
#include "nstring.h"
#include "platform.h"

#include <cassert>
#include <cstdio>

#ifdef NPLATFORM_LINUX

#include <limits.h> // IWYU pragma: keep
#include <stdlib.h> // IWYU pragma: keep
#include <sys/stat.h>
#include <unistd.h>

#elif NPLATFORM_WINDOWS

#include <direct.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#define chdir _chdir
#define PATH_MAX _MAX_PATH

#define realpath(n, r) _fullpath((r), (n), PATH_MAX)

#ifndef S_ISREG
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif // S_ISREG

#ifdef S_IFLNK
#ifndef S_ISLNK
#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif // S_ISLNK
#else
#define S_ISLNK(m) (0 && (m))
#endif // S_IFLNK

#ifndef S_ISDIR
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif // S_ISDIR

#endif // NPLATFORM_LINUX

namespace Novo {

bool fs_read_entire_file(Allocator* allocator, const String_Ref path, String* out_string)
{
    if (!fs_is_file(path)) {
        log_error("Not a regular file: %s\n", path.data);
        return false;
    }

    File_Handle file_handle;
    bool open_result = fs_open(path, FILE_MODE_READ, &file_handle);
    assert(open_result);

    u64 size;
    bool size_result = fs_size(&file_handle, &size);
    assert(size_result);

    out_string->data = nallocate_array(allocator, char, size + 1);
    assert(out_string->data);

    u64 read_size = fs_read(&file_handle, size, (u8*)out_string->data, &read_size);
    assert(read_size == 1);

    out_string->data[size] = '\0';
    out_string->length = size;

    fs_close(&file_handle);

    return true;
}

bool fs_write_entire_file(const String_Ref path, const String_Ref content)
{
    File_Handle file;
    if (!fs_open(path, File_Mode::FILE_MODE_WRITE, &file)) {
        log_error("Can't open/create file: %s\n", path.data);
    }

    u64 written;
    bool write_result = fs_write(&file, content.length, (u8*)content.data, &written);
    assert(write_result);
    assert(written == (u64)content.length);

    fs_close(&file);

    return false;
}

bool fs_open(const String_Ref path, File_Mode mode, File_Handle* out_handle)
{
    assert(out_handle);

    out_handle->handle = nullptr;
    out_handle->valid = false;

    bool read = (mode & FILE_MODE_READ) != 0;
    bool write = (mode & FILE_MODE_WRITE) != 0;

    const char* mode_str;

    if (read && write) {
        mode_str = "a+b";
    } else if (read && !write) {
        mode_str = "rb";
    } else if (!read && write) {
        mode_str = "wb";
    } else {
        log_error("Invalid mode passed to filesystem_open(): %s", path.data);
        return false;
    }

    FILE* file = fopen(path.data, mode_str);
    if (!file) {
        log_error("Error opening file: %s", path.data);
        return false;
    }

    if (write) {
        fseek(file, 0, SEEK_SET);
    }

    out_handle->handle = file;
    out_handle->valid = true;

    return true;
}

void fs_close(File_Handle* handle)
{
    assert(handle->valid);
    handle->valid = false;

    fclose((FILE*)handle->handle);
}

bool fs_size(File_Handle* handle, u64* out_size)
{
    assert(handle && handle->valid && handle->handle);
    assert(out_size);

    if (fseek((FILE*)handle->handle, 0, SEEK_END) == -1) {
        log_error("fseek failed....");
        return false;
    }

    *out_size = ftell((FILE*)handle->handle);
    rewind((FILE*)handle->handle);

    return true;
}

bool fs_read(File_Handle* handle, u64 size, u8* out_bytes, u64* out_size)
{
    assert(handle && handle->valid && handle->handle);
    assert(out_bytes && out_size);

    if (!size) {
        *out_size = 0;
        return true;
    }

    *out_size = fread(out_bytes, 1, size, (FILE*)handle->handle);
    if (*out_size != size) {
        return false;
    }

    return true;
}

bool fs_write(File_Handle* handle, u64 size, u8* bytes, u64* out_written)
{
    assert(handle && handle->valid && handle->handle);
    assert(size && out_written && bytes);

    size_t written = fwrite(bytes, size, 1, (FILE*)handle->handle);

    if (written != 1) {
        *out_written = 0;
        return false;
    }

    *out_written = size;
    return true;
}

bool fs_append(File_Handle* handle, u64 size, u8* bytes, u64* out_written)
{
    assert(handle && handle->valid && handle->handle);
    assert(size && out_written && bytes);

    int seek_res = fseek((FILE*)handle->handle, 0, SEEK_END);
    assert(seek_res == 0);

    return fs_write(handle, size, bytes, out_written);
}

bool fs_append(File_Handle* handle, const String_Ref str)
{
    u64 written;
    bool result = fs_append(handle, str.length, (u8*)str.data, &written);
    assert(written == (u64)str.length);

    return result;
}

bool fs_remove(const String_Ref path)
{
    NSTRING_ASSERT_ZERO_TERMINATION(path);
    int res = remove(path.data);
    return res == 0;
}

void fs_mkdir(const String_Ref path)
{
    NSTRING_ASSERT_ZERO_TERMINATION(path);
    platform_mkdir(path);
}

void fs_chdir(const String_Ref path)
{
    assert(path.length);

    NSTRING_ASSERT_ZERO_TERMINATION(path);

    int res = chdir(path.data);
    assert(res == 0);
}


bool fs_is_realpath(const String_Ref path)
{
    return platform_is_realpath(path);
}

String fs_realpath(Allocator* allocator, const String_Ref path)
{
    assert(path.length);

    NSTRING_ASSERT_ZERO_TERMINATION(path);

    char buf[PATH_MAX] ;

    auto result = realpath(path.data, buf);
    assert(result == buf && "realpath failed, input path might be invalid!");

    return string_copy(allocator, buf);
}

bool fs_is_directory(const String_Ref path)
{
    if (path.length <= 0) return false;

    NSTRING_ASSERT_ZERO_TERMINATION(path);

    struct stat sb;
    if (stat(path.data, &sb) == 0) {
        return S_ISDIR(sb.st_mode);
    }

    return false;
}

bool fs_is_file(const String_Ref path)
{
    if (path.length <= 0) return false;

    NSTRING_ASSERT_ZERO_TERMINATION(path);

    struct stat sb;
    if (stat(path.data, &sb) == 0) {
        return S_ISREG(sb.st_mode);
    }

    return false;
}

String fs_dirname(Allocator* allocator, const String_Ref path)
{
    return platform_dirname(allocator, path);
}

String fs_filename(Allocator* allocator, const String_Ref path)
{
    return platform_filename(allocator, path);
}

String fs_filename_strip_extension(Allocator* allocator, const String_Ref path)
{
    String filename = fs_filename(allocator, path);
    String result = filename;

    s64 last_dot_idx = string_last_index_of(filename, '.');
    if (last_dot_idx > 0) {
        result.length -= result.length - last_dot_idx;
        result.data[result.length] = '\0';
    }

    return result;
}

}

