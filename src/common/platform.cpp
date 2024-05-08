#include "platform.h"

#include "logger.h"
#include "memory/allocator.h"
#include "src/compiler/ssa.h"
#include "string_builder.h"

#include <assert.h>

#if NPLATFORM_LINUX

#include <limits.h> // IWYU pragma: keep
#include <libgen.h>
#include <unistd.h>
#include <cstdlib>
#include <sys/types.h>
#include <sys/wait.h>

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

Command_Result platform_run_command(Array_Ref<String_Ref> command_line)
{
    assert(command_line.count);

    auto ca = c_allocator();

    // Used for the pipes
    SECURITY_ATTRIBUTES sec_attr;
    sec_attr.nLength = sizeof(SECURITY_ATTRIBUTES);
    sec_attr.bInheritHandle = true;
    sec_attr.lpSecurityDescriptor = nullptr;

    HANDLE stdout_read_handle = nullptr;
    HANDLE stdout_write_handle = nullptr;
    HANDLE stderr_read_handle = nullptr;
    HANDLE stderr_write_handle = nullptr;

    if (!CreatePipe(&stdout_read_handle, &stdout_write_handle, &sec_attr, 0)) {
        ZFATAL("[platform_execute_process] Failed to create stdout pipe")
    }

    if (!SetHandleInformation(stdout_read_handle, HANDLE_FLAG_INHERIT, 0)) {
        ZFATAL("[platform_execute_process] SetHandleInformation failed for stdout pipe")
    }

    if (!CreatePipe(&stderr_read_handle, &stderr_write_handle, &sec_attr, 0)) {
        ZFATAL("[platform_execute_process] Failed to create stderr pipe")
    }

    if (!SetHandleInformation(stderr_read_handle, HANDLE_FLAG_INHERIT, 0)) {
        ZFATAL("[platform_execute_process] SetHandleInformation failed for stderr pipe")
    }

    PROCESS_INFORMATION process_info;
    STARTUPINFOW startup_info;
    ZeroMemory(&startup_info, sizeof(startup_info));
    ZeroMemory(&process_info, sizeof(process_info));

    startup_info.cb = sizeof(startup_info);
    startup_info.hStdOutput = stdout_write_handle;
    startup_info.hStdError = stderr_write_handle;
    startup_info.dwFlags |= STARTF_USESTDHANDLES;


    auto arg_str_ = string_append(ta, command_line, " ");
    Wide_String arg_str(ta, arg_str_);

    bool proc_res = CreateProcessW(nullptr, (LPWSTR)arg_str.data,
        nullptr, nullptr, true, 0, nullptr,
        nullptr, &startup_info, &process_info);

    Commanad_Result result = {};
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
        ZERROR("CreateProcessW failed with commandline: '%.*s", (int)arg_str.length, arg_str.data);
    }
    else {
        WaitForSingleObject(process_info.hProcess, INFINITE);

        CloseHandle(stdout_write_handle);
        CloseHandle(stderr_write_handle);

        String_Builder stdout_sb;
        string_builder_create(&stdout_sb, ta);

        String_Builder stderr_sb;
        string_builder_create(&stderr_sb, ta);

        for (;;) {
            const int buf_size = 1024;
            char buf_[buf_size];
            DWORD read_count;
            BOOL success = ReadFile(stdout_read_handle, buf_, buf_size, &read_count, nullptr);
            if (!success || read_count == 0) {
                break;
            }

            auto buf = platform_windows_normalize_line_endings(ta, String_Ref(buf_, read_count));
            string_builder_append(&stdout_sb, "%.*s", buf.length, buf.data);
        }

        for (;;) {
            const int buf_size = 1024;
            char buf_[buf_size];
            DWORD read_count;
            BOOL success = ReadFile(stderr_read_handle, buf_, buf_size, &read_count, nullptr);
            if (!success || read_count == 0) {
                break;
            }

            auto buf = platform_windows_normalize_line_endings(ta, String_Ref(buf_, read_count));
            string_builder_append(&stderr_sb, "%.*s", buf.length, buf.data);
        }

        DWORD exit_code;
        GetExitCodeProcess(process_info.hProcess, &exit_code);

        CloseHandle(process_info.hProcess);
        CloseHandle(process_info.hThread);

        result.exit_code = exit_code;
        result.success = result.exit_code == 0;

        if (stdout_sb.total_size) result.result_string = string_builder_to_string(ca, &stdout_sb);
        if (stderr_sb.total_size) result.error_string = string_builder_to_string(ca, &stderr_sb);

    }

    return result;
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
            const char* fmt = i > 0 ? " %.*s" : "%.*s";
            string_builder_append(&sb, fmt, (int)command_line[i].length, command_line[i].data);
        }

        string_builder_append(&sb, "'");

        String msg = string_builder_to_string(&sb);

        log_trace("%.*s", (int)msg.length, msg.data);
    }

    return _platform_run_command_(command_line);
}

}
