
compiler = executable('compiler', 'compiler.cpp',
                           include_directories: driver_includes, link_with: driver_libs)

test('compiler', compiler, suite: 'compiler', workdir: meson.current_source_dir())

