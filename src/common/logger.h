#pragma once

#include "defines.h"

namespace Novo {

enum Log_Level
{
    LOG_LEVEL_INVALID,

    LOG_LEVEL_TRACE,
    LOG_LEVEL_DEBUG,
    LOG_LEVEL_INFO,
    LOG_LEVEL_WARN,
    LOG_LEVEL_ERROR,
    LOG_LEVEL_FATAL,
};

extern Log_Level g_min_log_level;
extern Log_Level g_err_log_level;

NAPI void log_message(Log_Level level, const char *fmt, ...);

NAPI void log_trace(const char *fmt, ...);
NAPI void log_debug(const char *fmt, ...);
NAPI void log_info(const char *fmt, ...);
NAPI void log_warn(const char *fmt, ...);
NAPI void log_error(const char *fmt, ...);
NAPI void log_fatal(const char *fmt, ...);

}
