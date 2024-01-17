#include "logger.h"

#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>

namespace Novo {

static void log_message_va(Log_Level level, const char *fmt, va_list args)
{
    const char *label = nullptr;
    FILE *file = stdout;

    switch (level) {

        case LOG_LEVEL_INVALID: assert(false); break;

        case LOG_LEVEL_TRACE: {
            label = "[TRACE]";
            break;
        }

        case LOG_LEVEL_DEBUG: {
            label = "[DEBUG]";
            break;
        }

        case LOG_LEVEL_INFO: {
            label = "[INFO]";
            break;
        }

        case LOG_LEVEL_WARN: {
            label = "[WARN]";
            break;
        }

        case LOG_LEVEL_ERROR: {
            label = "[ERROR]";
            break;
        }

        case LOG_LEVEL_FATAL: {
            label = "[FATAL]";
            break;
        };
    }

    if (level < NOVO_MIN_LOG_LEVEL) {
        file = nullptr;
    } else if (level >= NOVO_ERR_LOG_LEVEL) {
        file = stderr;
    }

    if (file) {
        fprintf(file, "%s ", label);
        vfprintf(file, fmt, args);
        fprintf(file, "\n");
    }
}

#define LOG_MESSAGE(l, f) {         \
    va_list args;                   \
    va_start(args, f);              \
    log_message_va((l), (f), args); \
    va_end(args);                   \
}

void log_message(Log_Level level, const char *fmt, ...)
{
    LOG_MESSAGE(level, fmt);
}

void log_trace(const char *fmt, ...)
{
    LOG_MESSAGE(LOG_LEVEL_TRACE, fmt);
}

void log_debug(const char *fmt, ...)
{
    LOG_MESSAGE(LOG_LEVEL_DEBUG, fmt);
}

void log_info(const char *fmt, ...)
{
    LOG_MESSAGE(LOG_LEVEL_INFO, fmt);
}

void log_warn(const char *fmt, ...)
{
    LOG_MESSAGE(LOG_LEVEL_WARN, fmt);
}

void log_error(const char *fmt, ...)
{
    LOG_MESSAGE(LOG_LEVEL_ERROR, fmt);
}

void log_fatal(const char *fmt, ...)
{
    LOG_MESSAGE(LOG_LEVEL_FATAL, fmt);
    exit(1);
}

#undef LOG_MESSAGE

}
