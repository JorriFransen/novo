
project('dyncall', 'c', version: '1.4')

cc = meson.get_compiler('c')

system = host_machine.system()

prefix = join_paths(meson.current_source_dir(), 'install')
libdir = join_paths(prefix, 'lib')

if system == 'linux'

  run_command('configure', '--prefix=' + prefix, check: true)
  run_command('make', 'install', check: true)

  inc = include_directories('install/include')

elif system == 'windows'

  run_command('build_dyncall.bat', check: true)

  inc = include_directories('install\\include')

else
  assert(false, 'Unsupported platform!')
endif


deps = [
  cc.find_library('dyncall_s', dirs : [libdir]),
  cc.find_library('dyncallback_s', dirs : [libdir]),
  cc.find_library('dynload_s', dirs : [libdir]),
]

libdyncall_dep = declare_dependency(include_directories : inc, dependencies : deps)
