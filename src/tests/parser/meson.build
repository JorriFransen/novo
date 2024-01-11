
mem_alignment = executable('parser', 'parse.cpp',
                           include_directories: driver_includes, link_with: driver_libs)

test('parser', mem_alignment, suite: 'compiler', workdir: meson.current_source_dir())

