
project('novo', 'cpp', default_options: ['unity=on'])

common_source = [
  'src/common/defines.h',
  'src/common/memory/allocator.cpp', 'src/common/memory/allocator.h',
  'src/common/memory/linear_allocator.cpp', 'src/common/memory/linear_allocator.h',
]

common_includes = include_directories('src/common/')
common_lib = library('novo_common', sources: common_source, include_directories: common_includes)


compiler_source = [ 'src/compiler/instance.cpp', 'src/compiler/instance.h' ]
novo_lib = library('novo', sources: compiler_source, include_directories: common_includes, link_with: common_lib)

driver_source = [ 'src/driver/main.cpp']
driver_libs = [ common_lib, novo_lib ]
driver_exe = executable('novo', sources: driver_source, include_directories: common_includes, link_with: driver_libs)

subdir('tests')
