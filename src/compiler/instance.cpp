#include "instance.h"

#include <containers/darray.h>
#include <containers/stack.h>
#include <filesystem.h>
#include <logger.h>
#include <platform.h>

#include "ast.h"
#include "ast_print.h"
#include "atom.h"
#include "keywords.h"
#include "parser.h"
#include "resolver.h"
#include "scope.h"
#include "source_pos.h"
#include "ssa.h"
#include "task.h"
#include "type.h"
#include "typer.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

namespace Novo {

void instance_init(Instance* inst, Options options)
{
    inst->options = options;

    if (options.verbose) set_min_log_level(LOG_LEVEL_DEBUG);
    if (options.trace) set_min_log_level(LOG_LEVEL_TRACE);

    if (!fs_is_directory(inst->cwd)) {
        assert(false && "Invalid cwd!");
    }

    fs_chdir(inst->cwd);


    auto default_alloc = inst->default_allocator;

    inst->temp_allocator = temp_allocator_create(&inst->temp_allocator_data, default_alloc, MEBIBYTE(1));
    inst->ast_allocator = linear_allocator_create(&inst->ast_allocator_data, default_alloc, KIBIBYTE(64));
    inst->scope_allocator = inst->ast_allocator;

    darray_init(default_alloc, &inst->parse_tasks);
    darray_init(default_alloc, &inst->resolve_tasks);
    darray_init(default_alloc, &inst->type_tasks);
    darray_init(default_alloc, &inst->ssa_tasks);

    inst->global_scope = scope_new(inst, Scope_Kind::GLOBAL);

    darray_init(&inst->ast_allocator, &inst->imported_files);

    darray_init(&inst->ast_allocator, &inst->function_types);

    inst->fatal_error = false;

    if (!g_atoms_initialized) {
        initialize_atoms(default_alloc, 128);
        initialize_keywords();
        g_atoms_initialized = true;
    }

    // TODO: Custom allocator
    inst->ssa_program = allocate<SSA_Program>(c_allocator());
    ssa_program_init(inst->ssa_program, c_allocator());

    inst->builtin_type_void = void_type_new(inst);
    auto void_decl = ast_builtin_type_decl(inst, inst->builtin_type_void, "void");
    scope_add_symbol(inst->global_scope, void_decl->ident->atom, void_decl);

    inst->builtin_type_u8 = integer_type_new(inst, false, 8);
    auto u8_decl = ast_builtin_type_decl(inst, inst->builtin_type_u8, "u8");
    scope_add_symbol(inst->global_scope, u8_decl->ident->atom, u8_decl);

    inst->builtin_type_u16 = integer_type_new(inst, false, 16);
    auto u16_decl = ast_builtin_type_decl(inst, inst->builtin_type_u16, "u16");
    scope_add_symbol(inst->global_scope, u16_decl->ident->atom, u16_decl);

    inst->builtin_type_u32 = integer_type_new(inst, false, 32);
    auto u32_decl = ast_builtin_type_decl(inst, inst->builtin_type_u32, "u32");
    scope_add_symbol(inst->global_scope, u32_decl->ident->atom, u32_decl);

    inst->builtin_type_u64 = integer_type_new(inst, false, 64);
    auto u64_decl = ast_builtin_type_decl(inst, inst->builtin_type_u64, "u64");
    scope_add_symbol(inst->global_scope, u64_decl->ident->atom, u64_decl);

    inst->builtin_type_s8 = integer_type_new(inst, true, 8);
    auto s8_decl = ast_builtin_type_decl(inst, inst->builtin_type_s8, "s8");
    scope_add_symbol(inst->global_scope, s8_decl->ident->atom, s8_decl);

    inst->builtin_type_s16 = integer_type_new(inst, true, 16);
    auto s16_decl = ast_builtin_type_decl(inst, inst->builtin_type_s16, "s16");
    scope_add_symbol(inst->global_scope, s16_decl->ident->atom, s16_decl);

    inst->builtin_type_s32 = integer_type_new(inst, true, 32);
    auto s32_decl = ast_builtin_type_decl(inst, inst->builtin_type_s32, "s32");
    scope_add_symbol(inst->global_scope, s32_decl->ident->atom, s32_decl);

    inst->builtin_type_s64 = integer_type_new(inst, true, 64);
    auto s64_decl = ast_builtin_type_decl(inst, inst->builtin_type_s64, "s64");
    scope_add_symbol(inst->global_scope, s64_decl->ident->atom, s64_decl);

    inst->builtin_type_int = inst->builtin_type_s64;
    auto int_decl = ast_builtin_type_decl(inst, inst->builtin_type_int, "int");
    scope_add_symbol(inst->global_scope, int_decl->ident->atom, int_decl);

    inst->builtin_type_bool = boolean_type_new(inst, 8);
    auto bool_decl = ast_builtin_type_decl(inst, inst->builtin_type_bool, "bool");
    scope_add_symbol(inst->global_scope, bool_decl->ident->atom, bool_decl);

    if (fs_is_directory(options.install_dir)) {

        assert(fs_is_directory(options.install_dir));
        if (fs_is_realpath(options.install_dir)) {
            inst->compiler_install_dir = string_copy(inst->default_allocator, options.install_dir);
        } else {
            inst->compiler_install_dir = fs_realpath(inst->default_allocator, options.install_dir);
        }

    } else {
        String compiler_exe_path = platform_exe_path(inst->default_allocator, options.argv_0);
        log_trace("Compiler exe path: '%s'", compiler_exe_path.data);

        String compiler_exe_dir = platform_dirname(inst->default_allocator, compiler_exe_path);
        assert(fs_is_directory(compiler_exe_dir));
        log_trace("Compiler exe dir: '%s'", compiler_exe_dir.data);

        inst->compiler_install_dir = platform_dirname(inst->default_allocator, compiler_exe_dir);
        assert(fs_is_directory(inst->compiler_install_dir));
    }
    log_trace("Compiler install dir: '%s'", inst->compiler_install_dir.data);

    inst->module_dir = string_format(inst->default_allocator, "%s" NPLATFORM_PATH_SEPARATOR "%s", inst->compiler_install_dir.data, "modules");
    assert(fs_is_directory(inst->module_dir));
    log_trace("Module dir: '%s'", inst->module_dir.data);

    inst->builtin_module_path = string_format(inst->default_allocator, "%s" NPLATFORM_PATH_SEPARATOR "%s", inst->module_dir.data, "builtin.no");
    assert(fs_is_file(inst->builtin_module_path));
    log_trace("Builtin module path: '%s'", inst->builtin_module_path);

    inst->builtin_module_loaded = false;
    inst->type_string = nullptr;

    // TODO: Dynamic allocator
    hash_table_create(c_allocator(), &inst->ident_positions);
    hash_table_create(c_allocator(), &inst->decl_positions);
    hash_table_create(c_allocator(), &inst->function_body_positions);
    hash_table_create(c_allocator(), &inst->stmt_positions);
    hash_table_create(c_allocator(), &inst->expr_positions);
    hash_table_create(c_allocator(), &inst->ts_positions);
}

void instance_free(Instance* inst)
{

    darray_free(&inst->parse_tasks);
    darray_free(&inst->resolve_tasks);
    darray_free(&inst->type_tasks);
    darray_free(&inst->ssa_tasks);

    darray_free(&inst->imported_files);
    darray_free(&inst->function_types);

    ssa_program_free(inst->ssa_program);

    temp_allocator_free(&inst->temp_allocator_data);
    linear_allocator_free(&inst->ast_allocator_data);

    hash_table_free(&inst->ident_positions);
    hash_table_free(&inst->decl_positions);
    hash_table_free(&inst->function_body_positions);
    hash_table_free(&inst->stmt_positions);
    hash_table_free(&inst->expr_positions);
    hash_table_free(&inst->ts_positions);
}

bool instance_start(Instance* inst)
{
    auto first_file_name = String_Ref(inst->options.input_file);
    if (!first_file_name.length) {
        fprintf(stderr, "Input file not set!\n");
        return false;
    }

    return instance_start(inst, first_file_name);
}

bool instance_start(Instance* inst, String_Ref first_file_name, bool builtin_module/*=false*/)
{
    if (!inst->builtin_module_loaded && !builtin_module) {

        bool builtin_result = instance_start(inst, inst->builtin_module_path, true);
        if (!builtin_result) return false;

        AST_Declaration *string_decl = scope_find_symbol(inst->global_scope, atom_get("string"), nullptr);
        assert(string_decl);
        assert(string_decl->kind == AST_Declaration_Kind::STRUCT);
        inst->type_string = string_decl->resolved_type;

        inst->builtin_module_loaded = true;
    }

    String first_file_path;

    if (!fs_is_file(first_file_name)) {
        fprintf(stderr, "Invalid file path: %s\n", first_file_name.data);
        return false;
    }

    if (!fs_is_realpath(first_file_name)) {
        first_file_path = fs_realpath(&inst->ast_allocator, first_file_name);
    } else {
        first_file_path = string_copy(&inst->ast_allocator, first_file_name);
    }

    Atom path_atom = atom_get(first_file_name);
    add_parse_task(inst, path_atom);

    bool progress = true;

    while (progress) {

        progress = false;

        for (s64 i = 0; i < inst->parse_tasks.count; i++) {
            Parse_Task task = inst->parse_tasks[i];

            AST_File* parsed_file = parse_file(inst, atom_string(task.file_name), task.imported_file_index);
            if (parsed_file)
            {
                inst->imported_files[task.imported_file_index].ast = parsed_file;
                darray_remove_unordered(&inst->parse_tasks, i);
                i--;

            } else {
                assert(false); // Parser should have exited with a fatal error
            }

            progress = true;

            add_resolve_tasks(inst, parsed_file, inst->global_scope);
        }

        for (s64 i = 0; i < inst->resolve_tasks.count; i++) {
            Resolve_Task* task_ptr = &inst->resolve_tasks[i];
            auto task = *task_ptr;

            bool success = resolve_node(inst, task_ptr, &task_ptr->node, task_ptr->scope);

            if (success) {
                progress = true;

                darray_remove_unordered(&inst->resolve_tasks, i);
                i--;

                add_type_task(inst, task.node, task.scope, task.fn_decl);

                stack_free(&task.loop_control_stack);

            }
        }

        for (s64 i = 0; i < inst->type_tasks.count; i++) {
            Type_Task task = inst->type_tasks[i];

            bool success = type_node(inst, &task, &task.node, task.scope);

            if (success) {
                progress = true;

                darray_remove_unordered(&inst->type_tasks, i);
                i--;

                if (task.node.kind == AST_Node_Kind::DECLARATION &&
                    task.node.declaration->kind == AST_Declaration_Kind::FUNCTION) {

                    add_ssa_task(inst, task.node);

                }
            }
        }

        for (s64 i = 0; i < inst->ssa_tasks.count; i++) {
            SSA_Task task = inst->ssa_tasks[i];

            bool success;

            auto wait_for = &task.func_decl->function.wait_for_bytecode;
            for (s64 i = 0; i < wait_for->count; i++) {
                auto wait_node = (*wait_for)[i];
                assert(wait_node.kind == AST_Node_Kind::DECLARATION);
                auto decl = wait_node.declaration;
                assert(decl->kind == AST_Declaration_Kind::FUNCTION);
                assert(decl->ident);

                if (ssa_find_function(inst->ssa_program, decl->ident->atom, nullptr)) {
                    darray_remove_ordered(wait_for, i);
                    i--;
                }
            }

            success = wait_for->count == 0;

            if (success) {

                success = ssa_emit_function(inst, inst->ssa_program, task.func_decl);
                assert(success);

                progress = true;

                darray_remove_unordered(&inst->ssa_tasks, i);
                i--;
            }
        }
    }

    bool all_done = inst->parse_tasks.count == 0 &&
                    inst->resolve_tasks.count == 0 &&
                    inst->type_tasks.count == 0 &&
                    inst->ssa_tasks.count == 0;

    if (!progress && !all_done) {

        bool error_reported = false;

        for (s64 i = 0; i < inst->resolve_tasks.count; i++) {
            auto t = &inst->resolve_tasks[i];

            if (t->waiting_for) {
                auto name = atom_string(t->waiting_for->atom);
                Source_Pos pos = source_pos(inst, t->waiting_for);
                instance_error(inst, pos, "Reference to undecared identifier: '%s'", name.data);
                error_reported = true;
            }
        }

        assert(error_reported);
        return false;
    }

    if (!builtin_module && inst->options.print_ast) {

        for (s64 i = 0; i < inst->imported_files.count; i++) {
            assert(inst->imported_files[i].ast);
            String ast_str = ast_to_string(inst, inst->imported_files[i].ast, &inst->temp_allocator);
            printf("\n%s", ast_str.data);
        }

    }

    if (!builtin_module && inst->options.print_bytecode) {
        String ssa_str = ssa_to_string(inst, c_allocator(), inst->ssa_program);
        printf("\n%s\n", ssa_str.data);
    }

    return true;
}

static void instance_error_va(Instance* inst, Source_Pos pos, const char* fmt, va_list args)
{
    inst->fatal_error = true;

    Imported_File file = inst->imported_files[pos.file_index];
    String_Ref name = atom_string(file.path);

    Line_Info li = line_info(file.newline_offsets, pos.offset);

    fprintf(stderr, "%s:%d:%d: error: ", name.data, li.line, li.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

static void instance_error_note_va(Instance* inst, Source_Pos pos, const char* fmt, va_list args)
{
    inst->fatal_error = true;

    Imported_File file = inst->imported_files[pos.file_index];
    String_Ref name = atom_string(file.path);

    Line_Info li = line_info(file.newline_offsets, pos.offset);

    fprintf(stderr, "%s:%d:%d: note: ", name.data, li.line, li.offset);
    fprintf(stderr, "note: ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

void instance_error(Instance* inst, Source_Pos pos, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_error_va(inst, pos, fmt, args);

    va_end(args);
}

NAPI void instance_fatal_error(Instance* inst, Source_Pos pos, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_error_va(inst, pos, fmt, args);

    va_end(args);

    exit(1);
}

void instance_fatal_error_note(Instance* inst, Source_Pos pos, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_error_note_va(inst, pos, fmt, args);

    va_end(args);

    exit(1);
}

}
