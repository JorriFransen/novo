
project('novo', 'cpp',
        version: 'dev',
        default_options: ['cpp_std=c++20', 'werror=true', 'unity=off', 'unity_size=8' ])

compiler = meson.get_compiler('cpp')
compiler_id = compiler.get_id()

link_args = []


if compiler_id == 'clang-cl'
  add_global_arguments('-D_CRT_SECURE_NO_WARNINGS', language : 'cpp')
elif compiler_id == 'msvc'
  link_args = [ '-nodefaultlib:libcmt', '-nodefaultlib:libcmtd', '-nodefaultlib:msvcrt' ]
endif

dyncall_proj = subproject('dyncall')
dyncall_dep = dyncall_proj.get_variable('libdyncall_dep')

common_source = [
  'src/common/defines.h',
  'src/common/filesystem.cpp', 'src/common/filesystem.h',
  'src/common/hash.cpp', 'src/common/hash.h',
  'src/common/logger.cpp', 'src/common/logger.h',
  'src/common/nstring.cpp', 'src/common/nstring.h',
  'src/common/platform.cpp', 'src/common/platform.h',
  'src/common/string_builder.cpp', 'src/common/string_builder.h',

  'src/common/containers/darray.h',
  'src/common/containers/hash_table.h',
  'src/common/containers/stack.h',

  'src/common/memory/allocator.cpp', 'src/common/memory/allocator.h',
  'src/common/memory/linear_allocator.cpp', 'src/common/memory/linear_allocator.h',
  'src/common/memory/temp_allocator.cpp', 'src/common/memory/temp_allocator.h',
]

common_include_dirs = ['src/common/']
common_includes = include_directories(common_include_dirs)
common_lib = library('novo_common', sources: common_source, include_directories: common_includes, cpp_args: '-DNEXPORT', link_args: link_args)


compiler_include_dirs = ['src/compiler']
compiler_includes = [
  include_directories(compiler_include_dirs),
  include_directories(common_include_dirs, is_system: true)
]

compiler_source = [
  'src/compiler/const_resolver.cpp', 'src/compiler/const_resolver.h',
  'src/compiler/ast.cpp', 'src/compiler/ast.h',
  'src/compiler/ast_print.cpp', 'src/compiler/ast_print.h',
  'src/compiler/atom.cpp', 'src/compiler/atom.h',
  'src/compiler/backend.cpp', 'src/compiler/backend.h',
  'src/compiler/c_backend.cpp', 'src/compiler/c_backend.h',
  'src/compiler/ffi.cpp', 'src/compiler/ffi.h',
  'src/compiler/instance.cpp', 'src/compiler/instance.h',
  'src/compiler/keywords.cpp', 'src/compiler/keywords.h',
  'src/compiler/lexer.cpp', 'src/compiler/lexer.h',
  'src/compiler/options.cpp', 'src/compiler/options.h',
  'src/compiler/parser.cpp', 'src/compiler/parser.h',
  'src/compiler/resolver.cpp', 'src/compiler/resolver.h',
  'src/compiler/scope.cpp', 'src/compiler/scope.h',
  'src/compiler/source_pos.cpp', 'src/compiler/source_pos.h',
  'src/compiler/ssa.cpp', 'src/compiler/ssa.h',
  'src/compiler/task.cpp', 'src/compiler/task.h',
  'src/compiler/token.cpp', 'src/compiler/token.h',
  'src/compiler/type.cpp', 'src/compiler/type.h',
  'src/compiler/typer.cpp', 'src/compiler/typer.h',
  'src/compiler/vm.cpp', 'src/compiler/vm.h',

  'src/compiler/microsoft_craziness.cpp', 'src/compiler/microsoft_craziness.h'
]

compiler_lib = library('novo_compiler', sources: compiler_source, include_directories: compiler_includes, link_with: common_lib, cpp_args: '-DNEXPORT', link_args: link_args, dependencies: [dyncall_dep])


driver_include_dirs = ['src/driver/']
driver_includes = [
  include_directories(driver_include_dirs),
  include_directories(compiler_include_dirs, is_system: true),
  include_directories(common_include_dirs, is_system: true),
 ]

driver_source = [
  'src/driver/main.cpp',
  'src/driver/command_line_args.cpp', 'src/driver/command_line_args.cpp',
]

driver_libs = [ common_lib, compiler_lib ]
driver_exe = executable('novo', sources: driver_source, include_directories: driver_includes, link_with: driver_libs)


runtime_support_source = [
  'src/compiler/runtime_support.cpp', 'src/compiler/runtime_support.h',
]


runtime_support_includes = [
    include_directories(common_include_dirs, is_system: false),
]

support_lib_s = static_library('novo_runtime_support', sources: runtime_support_source, include_directories: runtime_support_includes, cpp_args: '-DNEXPORT', link_args: link_args)
support_lib_d = shared_library('novo_runtime_support', sources: runtime_support_source, include_directories: runtime_support_includes, cpp_args: '-DNEXPORT', link_args: link_args)

test_deps = [
  support_lib_s, support_lib_d,
]

subdir('src/tests')
