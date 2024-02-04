#pragma once

#include <options.h>

namespace Novo
{

Options parse_command_line(int argc, char* argv[], Options* default_opts = nullptr);

}
