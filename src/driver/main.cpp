
#include <defines.h>
// #include <instance.h>
// #include <logger.h>
// #include <memory/allocator.h>
// #include <string_builder.h>

// #include "command_line_args.h"

#include "memory/freelist.h"
#include <cassert>

using namespace Novo;

int main(int argc, char* argv[])
{
    Allocator* flalloc = fl_allocator();
    Freelist* fl = (Freelist*)flalloc->user_data;

    Freelist_Header* initial_node = fl->first_free;

    dump_graph(fl, "g0.dot");

    u64* p1 = allocate(flalloc, u64);
    dump_graph(fl, "g1.dot");
    u64* p2 = allocate_size(flalloc, 8, u64);
    dump_graph(fl, "g2.dot");

    assert(p1 != p2);

    release(flalloc, p1);
    dump_graph(fl, "g3.dot");

    u64* p3 = allocate_size(flalloc, 8, u64);
    dump_graph(fl, "g4.dot");
    assert(p3 == p1);

    release(flalloc, p3);
    dump_graph(fl, "g5.dot");
    release(flalloc, p2);
    dump_graph(fl, "g6.dot");

    assert(fl->first_free == initial_node);
    assert(fl->first_free->size == MEBIBYTE(8));


    // auto options = parse_command_line(argc, argv);

    // Instance instance;
    // instance_init(&instance, options);

    // if (!instance_start(&instance)) {
    //     return 1;
    // }

    // #ifdef NOVO_TRACE_ALLOC
    //     instance_free(&instance);
    //     release(c_allocator(), options.output);
    //     free_atoms();

    //     report_allocator_trace();
    // #endif //NOVO_TRACE_ALLOC
    return 0;
}
