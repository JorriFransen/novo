#include "command_line_args.h"

#include <logger.h>
#include <nstring.h>

#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace Novo {

struct Cmd_Opt_Parser;

static void handle_long_option(Options *result, const char *prog_name, int *p_i, char *argv[]);
static void handle_short_option(Cmd_Opt_Parser *cop);
static char *next_arg(Cmd_Opt_Parser *cop);
static void command_line_usage(FILE *file, const char *prog_name);
static void command_line_error(const char *prog_name, const char *fmt, ...);

enum class Option_Type
{
    BOOL,
    STRING,
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
    };
};

static Option_Info option_infos[] = {
#define NOVO_BOOL_OPTION(s, l,  v) { s, #l, offsetof(Options, l), Option_Type::BOOL, { .bool_value = v }},
#define NOVO_STRING_OPTION(s, l,  v) { s, #l, offsetof(Options, l), Option_Type::STRING, { .string_value = v }},

    ALL_NOVO_OPTIONS_X

#undef NOVO_BOOL_OPTION
#undef NOVO_STRING_OPTION
};

#define SET_OPTION(opt, offset, type, val) \
    *(type *)(((u8 *)opt) + (offset)) = val;


#define print_options(o) \
    printf("verbose: %s\n", (o).verbose ? "true" : "false"); \
    printf("output: %s\n", (o).output); \
    printf("another: %s\n", (o).another); \
    printf("\n");

struct Cmd_Opt_Parser
{
    char **args;
    int arg_count;
    int arg_index;

    const char *prog_name;
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
    cop.prog_name = prog_name;

    if (default_opts) {
        cop.result = *default_opts;
    } else {
        cop.result = default_options();
    }

    printf("Default options:\n");
    print_options(cop.result);


    const char *input_file = nullptr;

    while (cop.arg_index < cop.arg_count) {

        if (string_starts_with(cop.args[cop.arg_index], "--")) {
            handle_long_option(&cop.result, prog_name, &cop.arg_index, cop.args);

        } else if (string_starts_with(cop.args[cop.arg_index], "-")) {
            handle_short_option(&cop);

        } else {
            if (!input_file) {
                input_file = cop.args[cop.arg_index];
            } else {
                command_line_error(prog_name, "Input file already set to: '%s'", input_file);
            }
        }

        cop.arg_index++;
    }

    printf("Parsed options:\n");
    print_options(cop.result);

    return cop.result;
}

static void handle_long_option(Options *result, const char *prog_name, int *p_i, char *argv[])
{
    auto opt = argv[*p_i];
    auto length = strlen(opt);

    if (length < 3) {
        command_line_error(prog_name, "Invalid option: '%s'", opt);
    }

    auto name = &opt[2];
    bool match = false;

    for (size_t i = 0; i < sizeof(option_infos) / sizeof(option_infos[0]); i++) {

        auto &info = option_infos[i];

        if (string_equal(name, info.long_name)) {
            match = true;

            switch (info.type) {
                case Option_Type::BOOL: {

                }
                case Option_Type::STRING: assert(false); break;
            }
        }
    }

    if (!match) {
        command_line_error(prog_name, "Invalid option: '%c'", opt[1]);
    }
}

static void handle_short_option(Cmd_Opt_Parser *cop)
{
    auto opt = cop->args[cop->arg_index];
    auto length = strlen(opt);

    bool match = false;

    for (size_t i = 0; i < sizeof(option_infos) / sizeof(option_infos[0]); i++) {

        auto &info = option_infos[i];
        if (info.short_name == opt[1]) {
            match = true;

            switch (info.type) {

                case Option_Type::BOOL: {

                    if (length > 2) {
                        command_line_error(cop->prog_name, "Invalid length for short boolean option: '%s'", opt);
                    }

                    SET_OPTION(&cop->result, info.offset_in_option_struct, bool, !info.bool_value);
                    break;
                }

                case Option_Type::STRING: {
                    const char *value;
                    if (length > 2) {
                        value = &opt[2];
                    } else {
                        value = next_arg(cop);
                    }
                    SET_OPTION(&cop->result, info.offset_in_option_struct, const char *, value);
                    break;
                }
            }
            break;
        }

    }

    if (!match) {
        command_line_error(cop->prog_name, "Invalid option: '%c'", opt[1]);
    }
}

static char *next_arg(Cmd_Opt_Parser *cop)
{
    cop->arg_index++;
    if (cop->arg_index >= cop->arg_count) {
        command_line_error(cop->prog_name, "Unexpected end of arguments (after '%s')", cop->args[cop->arg_index - 1]);
    }

    return cop->args[cop->arg_index];
}

static void command_line_usage(FILE *file, const char *prog_name)
{
    fprintf(file, "usage: %s input_file [options]\n", prog_name);
}

static void command_line_error(const char *prog_name, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s: ", prog_name);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n\n");

    va_end(args);

    command_line_usage(stderr, prog_name);

    exit(1);
}

}
