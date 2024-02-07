
project('dyncall', 'c', version: '1.4')

cc = meson.get_compiler('c')

system = host_machine.system()

if system == 'linux'

  prefix = join_paths(meson.current_source_dir(), 'install')
  libdir = join_paths(prefix, 'lib')

  run_command('configure', '--prefix=' + prefix, check: true)
  run_command('make', 'install', check: true)

  inc = include_directories('install/include')

elif system == 'windows'

  run_command('configure.bat', '/tool-msvc', check: true)
  run_command('build_dyncall.bat', meson.current_build_dir(), check: true)

  libdir = join_paths(meson.current_build_dir(), 'lib')
  inc = include_directories('include')

else
  assert(false, 'Unsupported platform!')
endif


deps = [
  cc.find_library('dyncall_s', dirs : [libdir]),
  cc.find_library('dyncallback_s', dirs : [libdir]),
  cc.find_library('dynload_s', dirs : [libdir]),
]

libdyncall_dep = declare_dependency(include_directories : inc, dependencies : deps)
