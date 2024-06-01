#include "platform.h"

#include "logger.h"
#include "memory/allocator.h"
#include "src/compiler/ssa.h"
#include "string_builder.h"

#include <assert.h>

#if NPLATFORM_LINUX

#include <cstdlib>
#include <libgen.h>
#include <limits.h> // IWYU pragma: keep
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#elif NPLATFORM_WINDOWS
#include <windows.h>
#include <cstring>
#include <stdlib.h>
#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

namespace Novo
{

void platform_free_command_result(Command_Result *cres)
{
    if (cres->result_string.length) {
        free(c_allocator(), cres->result_string.data);
    }

    if (cres->error_string.length) {
        free(c_allocator(), cres->error_string.data);
    }
}

#if NPLATFORM_LINUX

String platform_dirname(Allocator* allocator, const String_Ref path)
{
    auto path_copy = string_copy(allocator, path);
    NSTRING_ASSERT_ZERO_TERMINATION(path_copy);

    char* result = dirname(path_copy.data);

    String result_copy = string_copy(allocator, result);

    if (!(allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
        free(allocator, path_copy.data);
    }

    return result_copy;
}

String platform_filename(Allocator* allocator, String_Ref path)
{
    auto path_copy = string_copy(allocator, path);
    NSTRING_ASSERT_ZERO_TERMINATION(path_copy);

    char* result = basename(path_copy.data);

    String result_copy = string_copy(allocator, result);

    if (!(allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
        free(allocator, path_copy.data);
    }

    return result_copy;
}

String platform_exe_path(Allocator* allocator, const char* argv_0)
{
    char exe_path[PATH_MAX];
    ssize_t exe_path_length = readlink("/proc/self/exe", exe_path, PATH_MAX);
    assert(exe_path_length != -1);

    return string_copy(allocator, exe_path, exe_path_length);
}

bool platform_is_realpath(const String_Ref path)
{
    if (path.length <= 0) return false;;

    NSTRING_ASSERT_ZERO_TERMINATION(path);

    if (path.length <= 0) return false;

    if (path[0] == '/') {
        struct stat sb;
        return stat(path.data, &sb) == 0;
    }

    return false;
}

void platform_mkdir(const String_Ref path)
{
    mkdir(path.data, 0700);
}

// TODO: I expect this has the same problem as the old windows version (deadlocking with full pipe(s)).
//       Use threads for now, like on windows. If/when the windows version changes to overlapped/async io,
//       this should be able to work in a similar way.
Command_Result _platform_run_command_(Array_Ref<String_Ref> command_line)
{
    assert(command_line.count);

#define NUM_PIPES   3
#define READ_FD     0
#define WRITE_FD    1
#define STDIN_PIPE  0
#define STDOUT_PIPE 1
#define STDERR_PIPE 2

#define PARENT_STDOUT_FD ( pipes[STDOUT_PIPE][READ_FD] )
#define PARENT_STDIN_FD  ( pipes[STDIN_PIPE][WRITE_FD] )
#define PARENT_STDERR_FD ( pipes[STDERR_PIPE][READ_FD] )

#define CHILD_STDOUT_FD ( pipes[STDOUT_PIPE][WRITE_FD] )
#define CHILD_STDIN_FD  ( pipes[STDIN_PIPE][READ_FD] )
#define CHILD_STDERR_FD ( pipes[STDERR_PIPE][WRITE_FD] )

    auto ca = c_allocator();

    int pipes[NUM_PIPES][2];

    int pres = 0;

    pres = pipe(pipes[STDIN_PIPE]);
    assert(pres == 0);
    pres = pipe(pipes[STDOUT_PIPE]);
    assert(pres == 0);
    pres = pipe(pipes[STDERR_PIPE]);
    assert(pres == 0);

    pid_t pid;

    if ((pid = fork()) == -1) {
        log_fatal("Fork error!");

    } else if (pid == 0) {
        // new child process

        dup2(CHILD_STDIN_FD, STDIN_FILENO);
        dup2(CHILD_STDOUT_FD, STDOUT_FILENO);
        dup2(CHILD_STDERR_FD, STDERR_FILENO);

        close(CHILD_STDIN_FD);
        close(CHILD_STDOUT_FD);
        close(CHILD_STDERR_FD);
        close(PARENT_STDOUT_FD);
        close(PARENT_STDIN_FD);
        close(PARENT_STDERR_FD);

        char** argv = allocate_array<char*>(ca, command_line.count + 1);

        for (s64 i = 0; i < command_line.count; i++) {
            argv[i] = (char*)command_line[i].data;
        }

        argv[command_line.count] = 0;

        execvp(argv[0], (char* const*)argv);

        String cmd_line_string = string_append(ca, command_line);
        log_fatal("execv error: '%s'", cmd_line_string.data);
        exit(1);

    } else {
        // Original parent process


        close(CHILD_STDIN_FD);
        close(CHILD_STDOUT_FD);
        close(CHILD_STDERR_FD);

        close(PARENT_STDIN_FD);


        String_Builder stdout_sb, stderr_sb;
        string_builder_init(&stdout_sb, ca);
        string_builder_init(&stderr_sb, ca);

        int exit_status;

        pid_t wait_res = waitpid(pid, &exit_status, 0);
        if (wait_res == -1) {
            log_fatal("Unexpected return value from waitpid");
        }


        bool stdout_written = false;
        { // Read stdout
            size_t read_count;
            char buffer[1024];
            do {
                read_count = read(PARENT_STDOUT_FD, buffer, sizeof(buffer) - 1);
                if (read_count) {
                    string_builder_append(&stdout_sb, "%.*s", read_count, buffer);
                    stdout_written = true;
                }
            } while (read_count > 0);
        }

        bool stderr_written = false;
        { // Read stderr
            size_t read_count;
            char buffer[1024];
            do {
                read_count = read(PARENT_STDERR_FD, buffer, sizeof(buffer) - 1);
                if (read_count) {
                    string_builder_append(&stderr_sb, "%.*s", read_count, buffer);
                    stderr_written = true;
                }
            } while (read_count > 0);
        }

        close(PARENT_STDOUT_FD);
        close(PARENT_STDERR_FD);

        Command_Result result = {};
        result.exit_code = WEXITSTATUS(exit_status);
        result.success = result.exit_code == 0;

        if (stdout_written) {
            result.result_string = string_builder_to_string(&stdout_sb, ca);
        }

        if (stderr_written) {
            result.error_string = string_builder_to_string(&stderr_sb, ca);
        }

        string_builder_free(&stdout_sb);
        string_builder_free(&stderr_sb);

        return result;
    }

#undef NUM_PIPES
#undef READ_FD
#undef WRITE_FD
#undef STDIN_PIPE
#undef STDOUT_PIPE
#undef STDERR_PIPE

#undef PARENT_STDOUT_FD
#undef PARENT_STDIN_FD
#undef PARENT_STDERR_FD

#undef CHILD_STDOUT_FD
#undef CHILD_STDIN_FD
#undef CHILD_STDERR_FD

    assert(false);
    return {};
}

#elif NPLATFORM_WINDOWS

String platform_dirname(Allocator* allocator, String_Ref path)
{
    s64 last_index = path.length;

    for (s64 i = path.length - 1; i >= 0; i--) {
        if (path[i] == '/' || path[i] == '\\') {
            last_index = i;
            break;
        }
    }

    if (last_index == 0) {
        return {};
    } else if (last_index < path.length) {
        return string_copy(allocator, path.data, last_index);
    }

    assert(last_index == path.length);
    return string_copy(allocator, path);
}

String platform_filename(Allocator* allocator, String_Ref path)
{
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];

    errno_t result = _splitpath_s(path.data,
                                  nullptr, 0, // drive
                                  nullptr, 0, // dir
                                  fname, _MAX_FNAME,
                                  ext, _MAX_EXT); // ext

    assert(result == 0);

    return string_append(allocator, fname, ext);
}

String platform_exe_path(Allocator* allocator, const char* argv_0)
{
    const size_t buf_length = 2048;
    char buf[buf_length];

    DWORD result = GetModuleFileNameA(nullptr, buf, buf_length);

    if (result >= 0 && result < buf_length) {
        return string_copy(allocator, buf, result);
    } else {
        DWORD err = GetLastError();
        if (err == ERROR_INSUFFICIENT_BUFFER) {
            assert(false && "[platform_exe_path] ERROR_INSUFFICIENT_BUFFER");
        } else {

            assert(false && "[platform_exe_path] Unknown error");
        }
    }

    assert(false);
    return {};
}

bool platform_is_realpath(const String_Ref path)
{
    char drive[_MAX_DRIVE];

    _splitpath_s(path.data,
                 drive, _MAX_DRIVE,
                 nullptr, 0, // dir
                 nullptr, 0, // fname
                 nullptr, 0) ; // ext

    return strlen(drive) > 0;
}

void platform_mkdir(const String_Ref path)
{
    CreateDirectoryA(path.data, nullptr);
}

struct Read_Thread_Data
{
    HANDLE read_handle;
    String_Builder* sb;
};

static DWORD WINAPI read_cmd_stdout(LPVOID data) {

    Read_Thread_Data* info = (Read_Thread_Data*)data;
    Allocator* ca = c_allocator();

    const size_t buf_size = 1024;
    char buf[buf_size + 1];

    DWORD read_count;
    BOOL read_result = ReadFile(info->read_handle, buf, buf_size, &read_count, nullptr);
    while (read_result == TRUE) {

        buf[read_count] = '\0';

        // NOTE: Using c allocator for thread safety
        // TODO: Change platform_windows_normalize_line_endings to modify the fixed buffer in place
        String str = platform_windows_normalize_line_endings(ca, String_Ref(buf, read_count));

        string_builder_append(info->sb, "%.*s", (int)str.length, str.data);
        free(ca, str.data);

        read_result = ReadFile(info->read_handle, buf, buf_size, &read_count, nullptr);
    }

    return 0;
}

// This is using 2 threads to read from the spawned processes stdin and stdout.
// String builders are used (with c_allocator() for thread safety) to combine and return the read reads.
// TODO: Use a thread save arena/dynamic allocator for the string builders.
// TODO: MAYBE use overlapped/async io and named pipes, get rid of the threading completely.
Command_Result _platform_run_command_(Array_Ref<String_Ref> command_line)
{
    assert(command_line.count);

    auto ta = temp_allocator();
    auto ca = c_allocator();
    auto mark = temp_allocator_get_mark();

    // Used for the pipes
    SECURITY_ATTRIBUTES sec_attr;
    sec_attr.nLength = sizeof(SECURITY_ATTRIBUTES);
    sec_attr.lpSecurityDescriptor = nullptr;
    sec_attr.bInheritHandle = true;

    HANDLE stdout_read_handle = nullptr;
    HANDLE stdout_write_handle = nullptr;
    HANDLE stderr_read_handle = nullptr;
    HANDLE stderr_write_handle = nullptr;

    if (!CreatePipe(&stdout_read_handle, &stdout_write_handle, &sec_attr, 0)) {
        log_fatal("[platform_execute_process] Failed to create stdout pipe");
    }

    if (!CreatePipe(&stderr_read_handle, &stderr_write_handle, &sec_attr, 0)) {
        log_fatal("[platform_execute_process] Failed to create stderr pipe");
    }

    STARTUPINFOW startup_info;
    ZeroMemory(&startup_info, sizeof(startup_info));
    startup_info.cb = sizeof(startup_info);
    startup_info.hStdOutput = stdout_write_handle;
    startup_info.hStdError = stderr_write_handle;
    startup_info.hStdInput = 0;
    startup_info.dwFlags |= STARTF_USESTDHANDLES;

    PROCESS_INFORMATION process_info;
    ZeroMemory(&process_info, sizeof(process_info));

    auto arg_str_ = string_append(ta, command_line, " ");
    Wide_String arg_str(ta, arg_str_);

    Command_Result result = {};

    BOOL proc_res = CreateProcessW(nullptr, arg_str.data,
                                   nullptr, nullptr,
                                   true, 0,
                                   nullptr, nullptr,
                                   &startup_info, &process_info);

    if (!proc_res) {
        auto err = GetLastError();
        LPSTR message_buf = nullptr;
        size_t size = FormatMessageA((FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_IGNORE_INSERTS),
            nullptr, err, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPSTR)&message_buf, 0, nullptr);


        result.error_string = string_copy(c_allocator(), String_Ref(message_buf, size));
        LocalFree(message_buf);

        result.success = false;
        log_error("CreateProcessW failed with commandline: '%.*s'", (int)arg_str_.length, arg_str_.data);

        return result;
    }

    CloseHandle(process_info.hProcess);
    CloseHandle(process_info.hThread);

    CloseHandle(stdout_write_handle);
    CloseHandle(stderr_write_handle);

    String_Builder stdout_sb;
    string_builder_init(&stdout_sb, ca); // NOTE: Using c allocator for thread safety

    String_Builder stderr_sb;
    string_builder_init(&stderr_sb, ca); // NOTE: Using c allocator for thread safety

    HANDLE read_threads[2];

    Read_Thread_Data stdout_thread_data = { stdout_read_handle, &stdout_sb };
    read_threads[0]= CreateThread(nullptr, 0, read_cmd_stdout, &stdout_thread_data, 0, nullptr);
    assert(read_threads[0]);

    Read_Thread_Data stderr_thread_data = { stderr_read_handle, &stderr_sb };
    read_threads[1] = CreateThread(nullptr, 0, read_cmd_stdout, &stderr_thread_data, 0, nullptr);
    assert(read_threads[1]);

    WaitForMultipleObjects(2, read_threads, TRUE, INFINITE);
    CloseHandle(stdout_read_handle);
    CloseHandle(stderr_read_handle);


    DWORD exit_code = GetExitCodeProcess(process_info.hProcess, &exit_code);
    result.exit_code = exit_code;
    result.success = result.exit_code == 0;
    result.result_string = string_builder_to_string(&stdout_sb, ca);
    result.error_string = string_builder_to_string(&stderr_sb, ca);

    string_builder_free(&stdout_sb);
    string_builder_free(&stderr_sb);

    temp_allocator_reset(mark);

    return result;
}

String platform_windows_normalize_line_endings(Allocator* allocator, const String_Ref str)
{
    s64 cr_count = 0;
    for (s64 i = 0; i < str.length - 1; i++) {
        if (str[i] == '\r' && str[i + 1] == '\n') {
            cr_count += 1;
        }
    }

    if (!cr_count) {
        return string_copy(allocator, str);
    }

    auto new_length = str.length - cr_count;
    auto buf = allocate_array<char>(allocator, new_length + 1);

    s64 read_index = 0;
    s64 copy_length = 0;
    char *write_cursor = buf;

    for (s64 i = 0; i < str.length; i++) {
        if (i < str.length - 1 && str[i] == '\r' && str[i + 1] == '\n') {
            memcpy(write_cursor, &str[read_index], copy_length);
            write_cursor += copy_length;
            copy_length = 0;
            read_index = i + 1;
        } else {
            copy_length += 1;
        }
    }

    if (copy_length) {
        memcpy(write_cursor, &str[read_index], copy_length);
    }

    return string(buf, new_length);
}

#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

Command_Result platform_run_command(Array_Ref<String_Ref> command_line, Allocator* debug_allocator) {

    if (debug_allocator) {
        String_Builder sb;
        string_builder_init(&sb, debug_allocator);

        string_builder_append(&sb, "Executing external command: '");

        for (s64 i = 0; i < command_line.count; i++) {
            if (command_line[i].length) {
                const char* fmt = i > 0 ? " %.*s" : "%.*s";
                string_builder_append(&sb, fmt, (int)command_line[i].length, command_line[i].data);
            }
        }

        string_builder_append(&sb, "'");

        String msg = string_builder_to_string(&sb);

        log_debug("%.*s", (int)msg.length, msg.data);
    }

    return _platform_run_command_(command_line);
}

}
