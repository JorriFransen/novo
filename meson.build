
project('novo', 'cpp',
        version: 'dev',
        default_options: 'unity=on')

common_source = [
  'src/common/defines.h',
  'src/common/nstring.cpp', 'src/common/nstring.h',
  'src/common/platform.cpp', 'src/common/platform.h',

  'src/common/containers/darray.h',

  'src/common/memory/allocator.cpp', 'src/common/memory/allocator.h',
  'src/common/memory/linear_allocator.cpp', 'src/common/memory/linear_allocator.h',
  'src/common/memory/temp_allocator.cpp', 'src/common/memory/temp_allocator.h',
]

common_includes = include_directories('src/common/')
common_lib = library('novo_common', sources: common_source, include_directories: common_includes)


compiler_includes = include_directories('src/compiler/')

compiler_source = [
  'src/compiler/instance.cpp', 'src/compiler/instance.h',
  'src/compiler/lexer.cpp', 'src/compiler/lexer.h',
  'src/compiler/parser.cpp', 'src/compiler/parser.h',
  'src/compiler/source_pos.h',
  'src/compiler/task.cpp', 'src/compiler/task.h',
]

novo_lib = library('novo', sources: compiler_source, include_directories: common_includes, link_with: common_lib)

driver_includes = [ common_includes, compiler_includes ]
driver_source = [ 'src/driver/main.cpp']
driver_libs = [ common_lib, novo_lib ]
driver_exe = executable('novo', sources: driver_source, include_directories: driver_includes, link_with: driver_libs)

subdir('src/tests')
