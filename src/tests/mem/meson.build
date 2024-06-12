
mem_freelist = executable('mem_freelist', 'test_freelist.cpp',
                          include_directories: driver_includes, link_with: driver_libs)

mem_alignment = executable('mem_alignment', 'test_alignment.cpp',
                           include_directories: driver_includes, link_with: driver_libs)


test('mem_freelist', mem_freelist, suite: 'memory')
test('mem_alignment', mem_alignment, suite: 'memory')

