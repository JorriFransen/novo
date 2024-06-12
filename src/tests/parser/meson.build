
parser = executable('parser', 'test_parse.cpp',
                           include_directories: driver_includes, link_with: driver_libs)

test('parser', parser, suite: 'compiler', workdir: meson.current_source_dir(), depends: test_deps)

