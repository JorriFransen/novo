
project('dyncall', 'c', version: '1.4')

cc = meson.get_compiler('c')

prefix = join_paths(meson.current_source_dir(), 'install')

run_command('configure', '--prefix=' + prefix, check: true)
run_command('make', 'install', check: true)

inc = include_directories('install/include')

libdir = join_paths(prefix, 'lib')
deps = [
  cc.find_library('dyncall_s', dirs : [libdir]),
  cc.find_library('dyncallback_s', dirs : [libdir]),
  cc.find_library('dynload_s', dirs : [libdir]),
]
libdyncall_dep = declare_dependency(include_directories : inc, dependencies : deps)
