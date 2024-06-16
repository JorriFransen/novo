#include "command_line_args.h"

#include <defines.h>
#include <filesystem.h>
#include <logger.h>
#include <memory/c_allocator.h>
#include <memory/arena.h>
#include <nstring.h>
#include <platform.h>

#include <cassert>
#include <cctype>
#include <cstdarg>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace Novo {

struct Cmd_Opt_Parser;

static void handle_long_option(Cmd_Opt_Parser *cop);
static void handle_short_option(Cmd_Opt_Parser *cop);
static void command_line_usage(FILE *file, const char *prog_name, bool suggest_help);
static void command_line_error(Cmd_Opt_Parser *cop, const char *fmt, ...);
static void command_line_help(FILE *file, Cmd_Opt_Parser *cop);

#define OPTION_CALLBACK_FN(n) void (n)(Cmd_Opt_Parser *cop)
typedef OPTION_CALLBACK_FN(Option_Callback_FN);
static OPTION_CALLBACK_FN(command_line_help_callback);

#undef CALLBACK // windows...

enum class Option_Type
{
    BOOL,
    STRING,

    CALLBACK,
};

struct Option_Info
{
    char short_name;
    const char *long_name;
    size_t offset_in_option_struct;
    Option_Type type;

    union {
        bool bool_value;
        const char *string_value;
        Option_Callback_FN *callback;
    };

    const char *description;
    const char *arg_info;
};

static Option_Info option_infos[] = {
#define NOVO_BOOL_OPTION(short_name, long_name, value, desc, arg_name) \
    { short_name, #long_name, offsetof(Options, long_name), Option_Type::BOOL, { .bool_value = value }, (desc), #long_name " " arg_name },

#define NOVO_STRING_OPTION(short_name, long_name,  value, desc, arg_name) \
    { short_name, #long_name, offsetof(Options, long_name), Option_Type::STRING, { .string_value = value }, (desc), #long_name " " arg_name },

    ALL_NOVO_OPTIONS_X

#undef NOVO_BOOL_OPTION
#undef NOVO_STRING_OPTION

    { 'h', "help", 0, Option_Type::CALLBACK, { .callback = command_line_help_callback }, "Print this help message", "help" },
};

#define SET_OPTION(opt, offset, type, val) \
    *(type*)(((u8*)opt) + (offset)) = val;


struct Cmd_Opt_Parser
{
    char **args;
    int arg_count;
    int arg_index;

    Options result;
};

Options parse_command_line(int argc, char *argv[], Options *default_opts/*=nullptr*/)
{

    const char *prog_name = argv[0];

    // Skip program name
    argc--;
    argv++;

    Cmd_Opt_Parser cop;
    cop.args = argv;
    cop.arg_count = argc;
    cop.arg_index = 0;

    if (default_opts) {
        cop.result = *default_opts;
    } else {
        cop.result = default_options();
    }

    cop.result.argv_0 = prog_name;

    while (cop.arg_index < cop.arg_count) {

        if (string_starts_with(cop.args[cop.arg_index], "--")) {
            handle_long_option(&cop);

        } else if (string_starts_with(cop.args[cop.arg_index], "-")) {
            handle_short_option(&cop);

        } else {
            if (!cop.result.input_file) {
                cop.result.input_file = cop.args[cop.arg_index];
            } else {
                command_line_error(&cop, "Input file already set to: '%s'", cop.result.input_file);
            }
        }

        cop.arg_index++;
    }

    if (!cop.result.input_file) {
        command_line_error(&cop, "Expected input file");
    }

    if (!fs_is_file(cop.result.input_file)) {
        fprintf(stderr, "Invalid file path: %s\n", cop.result.input_file);
        exit(1);
    }

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    String_Ref out_file_name = cop.result.output;
    if (!out_file_name.length) {
        out_file_name = fs_filename_strip_extension(&ta, cop.result.input_file);
        NSTRING_ASSERT_ZERO_TERMINATION(out_file_name);
    }

    if (!string_ends_with(out_file_name, NPLATFORM_DEFAULT_EXE_EXTENSION)) {
        out_file_name = string_append(&ta, out_file_name, NPLATFORM_DEFAULT_EXE_EXTENSION);
    }

    // TODO: Move extension stripping to instance init
    cop.result.output = string_copy(c_allocator(), out_file_name).data;

    temp_arena_release(tarena);

    return cop.result;
}

static void handle_long_option(Cmd_Opt_Parser *cop)
{
    auto opt = cop->args[cop->arg_index];
    auto length = strlen(opt);

    if (length < 3) {
        command_line_error(cop, "Invalid option: '%s'", opt);
    }

    auto name = &opt[2];
    bool match = false;

    for (size_t i = 0; i < sizeof(option_infos) / sizeof(option_infos[0]); i++) {

        auto &info = option_infos[i];

        // 'name' may contain more characters then just the option name.
        // EG. verbose=true
        if (string_equal(name, info.long_name) || (string_contains(name, '=') && string_starts_with(name, info.long_name))) {
            match = true;

            auto long_name_len = strlen(info.long_name);
            char *str_val = nullptr;
            bool no_argument = false;

            // Look for '=' first
            if (strlen(name) > long_name_len) {
                if (name[long_name_len] == '=') {

                    str_val = &name[long_name_len + 1];

                } else {
                    command_line_error(cop, "Expected '=' after --%s", info.long_name);
                }

            } else {

                // Check if the next argument is an option, if not use it as value
                if (cop->arg_index < cop->arg_count - 1 && cop->args[cop->arg_index + 1][0] != '-') {

                    str_val = cop->args[cop->arg_index + 1];
                    cop->arg_index += 1;

                } else {
                    no_argument = true;
                }
            }

            switch (info.type) {

                case Option_Type::BOOL: {

                    bool new_value = false;
                    if (str_val) {
                        for (auto p = str_val ; *p; ++p) *p = tolower(*p);
                        if (string_equal(str_val, "true")) {
                            new_value = true;
                        } else if (string_equal(str_val, "false")) {
                            new_value = false;
                        } else {
                            command_line_error(cop, "Invalid boolean value in option: '%s'", name);
                        }
                    } else {
                        assert(no_argument);
                        new_value = !info.bool_value;
                    }


                    SET_OPTION(&cop->result, info.offset_in_option_struct, bool, new_value);
                    break;
                }

                case Option_Type::STRING: {

                    if (no_argument) {
                        command_line_error(cop, "Expected argument after option: '%s'", name);
                    }

                    SET_OPTION(&cop->result, info.offset_in_option_struct, const char *, str_val);
                    break;
                }

                case Option_Type::CALLBACK: {
                    if (!no_argument) {
                        command_line_error(cop, "Option '%s' does not accept any arguments", name);
                    }

                    info.callback(cop);
                    break;
                }
            }

            break;
        }
    }

    if (!match) {
        command_line_error(cop, "Invalid option: '%s'", name);
    }
}

static void handle_short_option(Cmd_Opt_Parser *cop)
{

    auto chars = &cop->args[cop->arg_index][1];
    bool handled_arg = false;

    while (*chars && !handled_arg) {

        bool match = false;
        for (size_t i = 0; i < sizeof(option_infos) / sizeof(option_infos[0]); i++) {
            auto &info = option_infos[i];

            if (info.short_name == 0) continue;

            if (info.short_name == chars[0]) {
                match = true;

                switch (info.type) {
                    case Option_Type::BOOL: {
                        SET_OPTION(&cop->result, info.offset_in_option_struct, bool, !info.bool_value);
                        break;
                    }

                    case Option_Type::STRING: {
                        const char *value = nullptr;

                        if (chars[1]) {
                            value = &chars[1];
                        } else if (cop->arg_index < cop->arg_count - 1 && cop->args[cop->arg_index + 1][0] != '-') {
                            cop->arg_index += 1;
                            value = cop->args[cop->arg_index];
                        } else {
                            command_line_error(cop, "Expected argument after option '%c'", info.short_name);
                        }

                        handled_arg = true;
                        SET_OPTION(&cop->result, info.offset_in_option_struct, const char *, value);
                        break;
                    }

                    case Option_Type::CALLBACK: {
                        info.callback(cop);
                        break;
                    }

                }

                if (match) break;
            }
        }


        if (!match) {
            command_line_error(cop, "Invalid option: '%c'", chars[0]);
        }

        chars++;
    }
}

static void command_line_usage(FILE *file, const char *prog_name, bool suggest_help)
{
    fprintf(file, "Usage: %s input_file [options]\n", prog_name);

    if (suggest_help) fprintf(file, "       %s -h\tto display all options\n\n" ,prog_name);
}

static void command_line_error(Cmd_Opt_Parser *cop, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s: ", cop->result.argv_0);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n\n");

    va_end(args);

    command_line_usage(stderr, cop->result.argv_0, true);

    exit(1);
}

static OPTION_CALLBACK_FN(command_line_help_callback)
{
    command_line_help(stdout, cop);
    exit(0);
}

static void command_line_help(FILE *file, Cmd_Opt_Parser *cop)
{
    command_line_usage(file, cop->result.argv_0, false);

    fprintf(file, "\nOptions:\n");

    for (size_t i = 0; i < sizeof(option_infos) / sizeof(option_infos[0]); i++) {
        auto &info = option_infos[i];
        char short_name = info.short_name;
        if (!short_name) {
            fprintf(file, "      --%-32s", info.arg_info);
        } else {
            fprintf(file, "  -%c, --%-32s", short_name, info.arg_info);
        }


        if (info.description) {
            fprintf(file, "%s", info.description);
        }

        fprintf(file, "\n");
    }

    fprintf(file, "\n");
}

}
