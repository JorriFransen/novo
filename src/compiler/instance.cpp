#include "instance.h"

#include <containers/darray.h>
#include <containers/stack.h>
#include <filesystem.h>
#include <logger.h>
#include <nstring.h>
#include <platform.h>

#include "ast.h"
#include "ast_print.h"
#include "atom.h"
#include "backend.h"
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
    darray_init(default_alloc, &inst->run_tasks);

    inst->global_scope = scope_new(inst, Scope_Kind::GLOBAL);

    darray_init(&inst->ast_allocator, &inst->imported_files);
    inst->insert_file_index = -1;
    inst->inserted_strings_path = "inserted_strings.no";

    if (fs_is_file(inst->inserted_strings_path)) {
        fs_remove(inst->inserted_strings_path);
    }

    darray_init(&inst->ast_allocator, &inst->function_types);
    darray_init(&inst->ast_allocator, &inst->struct_types);
    darray_init(&inst->ast_allocator, &inst->enum_types);

    inst->fatal_error = false;

    if (!g_atoms_initialized) {
        initialize_atoms(default_alloc, 128);
        initialize_keywords();
        g_atoms_initialized = true;
    }

    if (inst->options.exe_dir.length) {
        assert(fs_is_realpath(inst->options.exe_dir));
        inst->compiler_exe_dir = fs_realpath(inst->default_allocator, inst->options.exe_dir);
    } else {
        String compiler_exe_path = platform_exe_path(inst->default_allocator, options.argv_0);
        log_trace("Compiler exe path: '%s'", compiler_exe_path.data);

        inst->compiler_exe_dir = platform_dirname(inst->default_allocator, compiler_exe_path);
    }

    assert(fs_is_directory(inst->compiler_exe_dir));
    log_trace("Compiler exe dir: '%s'", inst->compiler_exe_dir.data);

    inst->support_lib_s_path = string_format(inst->default_allocator, "%.*s" NPLATFORM_PATH_SEPARATOR NPLATFORM_STATIC_LIB_PREFIX "novo_runtime_support" NPLATFORM_STATIC_LIB_EXTENSION,
                                             (int)inst->compiler_exe_dir.length, inst->compiler_exe_dir.data);
    log_trace("Static support lib: '%s'", inst->support_lib_s_path.data);
    assert(fs_is_file(inst->support_lib_s_path));

    inst->support_lib_d_path = string_format(inst->default_allocator, "%.*s" NPLATFORM_PATH_SEPARATOR NPLATFORM_DYNAMIC_LIB_PREFIX "novo_runtime_support" NPLATFORM_DYNAMIC_LIB_EXTENSION,
                                             (int)inst->compiler_exe_dir.length, inst->compiler_exe_dir.data);
    log_trace("Dynamic support lib: '%s'", inst->support_lib_d_path.data);
    assert(fs_is_file(inst->support_lib_d_path));

    String current_search_dir = inst->compiler_exe_dir;
    bool module_dir_found = false;
    while (!module_dir_found) {
        String parent_dir = platform_dirname(&inst->temp_allocator, current_search_dir);

#if NPLATFORM_LINUX
        if (parent_dir.length == 0 || string_equal(parent_dir, "/")) break;
#elif NPLATFORM_WINDOWS
        if (parent_dir.length <= 3) break;
#endif // NPLATFORM_LINUX

        String candidate = string_format(&inst->temp_allocator, "%.*s" NPLATFORM_PATH_SEPARATOR "modules", (int)parent_dir.length, parent_dir.data);

        if (fs_is_directory(candidate)) {
            module_dir_found = true;
            inst->module_dir = string_copy(inst->default_allocator, candidate);
            break;
        }

        current_search_dir = parent_dir;
    }

    if (!module_dir_found) log_fatal("Unable to find module diretory!");
    log_trace("Module dir: '%s'", inst->module_dir.data);

    inst->builtin_module_path = string_format(inst->default_allocator, "%.*s" NPLATFORM_PATH_SEPARATOR "%s", (int)inst->module_dir.length, inst->module_dir.data, "builtin.no");
    assert(fs_is_file(inst->builtin_module_path));
    log_trace("Builtin module path: '%s'", inst->builtin_module_path.data);

    inst->builtin_module_loaded = false;

    // TODO: Custom allocator
    inst->ssa_program = allocate<SSA_Program>(c_allocator());
    ssa_program_init(inst->ssa_program, c_allocator());

    // TODO: Custom allocator
    vm_init(&inst->vm, c_allocator(), inst);

    assert(sizeof(void*) == 8);
    inst->pointer_byte_size = 8;

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

    inst->builtin_type_cchar = integer_type_new(inst, false, 8);

    inst->builtin_type_cstring = pointer_type_new(inst, inst->builtin_type_cchar);
    auto cstring_decl = ast_builtin_type_decl(inst, inst->builtin_type_cstring, "cstring");
    scope_add_symbol(inst->global_scope, cstring_decl->ident->atom, cstring_decl);

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
    darray_free(&inst->run_tasks);

    darray_free(&inst->imported_files);
    darray_free(&inst->function_types);
    darray_free(&inst->struct_types);

    ssa_program_free(inst->ssa_program);
    vm_free(&inst->vm);

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

            AST_File* parsed_file = nullptr;
            DArray<AST_Node> nodes = {};

            if (task.kind == Parse_Task_Kind::FILE) {
                parsed_file = parse_file(inst, atom_string(task.name), task.imported_file_index);
            } else {
                assert(task.kind == Parse_Task_Kind::INSERT);
                nodes = parse_string(inst, atom_string(task.name), task.content, task.insert.scope, task.context, task.imported_file_index, task.offset);
            }

            if (parsed_file || nodes.count)
            {
                if (parsed_file) inst->imported_files[task.imported_file_index].ast = parsed_file;

                darray_remove_unordered(&inst->parse_tasks, i);
                i--;

            } else {
                assert(false); // Parser should have exited with a fatal error
            }

            progress = true;

            if (parsed_file) {
                add_resolve_tasks(inst, parsed_file, inst->global_scope, nullptr);
            } else {
                assert(task.insert.scope);
                add_resolve_tasks(inst, nodes, task.insert.scope, task.insert.fn_decl, task.insert.bc_deps);
                task.insert.stmt->insert.nodes_to_insert = nodes;
            }
        }

        for (s64 i = 0; i < inst->resolve_tasks.count; i++) {
            Resolve_Task* task_ptr = &inst->resolve_tasks[i];
            auto task = *task_ptr;

            bool success = resolve_node(inst, task_ptr, &task_ptr->node, task_ptr->scope);

            if (success) {
                progress = true;

                darray_remove_unordered(&inst->resolve_tasks, i);
                i--;

                add_type_task(inst, task.node, nullptr, task.scope, task.fn_decl, task.bytecode_deps);

                stack_free(&task.loop_control_stack);

            }
        }

        for (s64 i = 0; i < inst->type_tasks.count; i++) {
            Type_Task* task = &inst->type_tasks[i];

            bool success = type_node(inst, task, &task->node, task->scope);

            if (success) {
                progress = true;

                if (task->node.kind == AST_Node_Kind::DECLARATION) {

                    if (task->node.declaration->kind == AST_Declaration_Kind::FUNCTION ||
                        (task->node.declaration->kind == AST_Declaration_Kind::VARIABLE && task->node.declaration->flags & AST_DECL_FLAG_GLOBAL) ||
                        (task->node.declaration->kind == AST_Declaration_Kind::CONSTANT && task->node.declaration->resolved_type->kind == Type_Kind::STRUCT)) {

                        add_ssa_task(inst, task->node.declaration, task->bytecode_deps, nullptr);
                    }

                } else if (task->node.kind == AST_Node_Kind::STATEMENT) {

                    auto stmt = task->node.statement;
                    if (stmt->kind == AST_Statement_Kind::DECLARATION && stmt->declaration->kind == AST_Declaration_Kind::CONSTANT && stmt->declaration->resolved_type->kind == Type_Kind::STRUCT) {

                        add_ssa_task(inst, stmt->declaration, task->bytecode_deps, nullptr);
                    }
                }

                darray_remove_unordered(&inst->type_tasks, i);
                i--;

            }
        }

        for (s64 i = 0; i < inst->ssa_tasks.count; i++) {
            SSA_Task task = inst->ssa_tasks[i];

            if (task.bytecode_deps) {

                for (s64 i = 0; i < task.bytecode_deps->count; i++) {
                    auto wait_node = (*task.bytecode_deps)[i];

                    if (wait_node.kind == AST_Node_Kind::DECLARATION) {
                        auto decl = wait_node.declaration;
                        assert(decl->ident);

                        if (decl->kind == AST_Declaration_Kind::FUNCTION) {

                            if (ssa_find_function(inst->ssa_program, decl->ident->atom, nullptr)) {
                                darray_remove_ordered(task.bytecode_deps, i);
                                i--;
                            }

                        } else {
                            assert(decl->kind == AST_Declaration_Kind::VARIABLE);
                            assert(decl->flags & AST_DECL_FLAG_GLOBAL);

                            if (ssa_find_global_variable(inst->ssa_program, decl->ident->atom, nullptr)) {
                                darray_remove_ordered(task.bytecode_deps, i);
                                i--;
                            }

                        }

                    } else if (wait_node.kind == AST_Node_Kind::EXPRESSION) {
                        assert(wait_node.expression->kind == AST_Expression_Kind::RUN);

                        if (wait_node.expression->run.generated_expression) {
                            darray_remove_ordered(task.bytecode_deps, i);
                            i--;
                        }
                    } else {
                        assert(wait_node.kind == AST_Node_Kind::STATEMENT);
                        assert(wait_node.statement->kind == AST_Statement_Kind::INSERT);

                        if (wait_node.statement->insert.nodes_to_insert.count) {
                            darray_remove_ordered(task.bytecode_deps, i);
                            i--;
                        }
                    }
                }

                if (task.bytecode_deps->count != 0) continue;
            }

            bool success;

            if (task.kind == SSA_Task_Kind::RUN || task.kind == SSA_Task_Kind::INSERT) {

                s64 wrapper_index = ssa_emit_run_wrapper(inst, inst->ssa_program, task.node, task.scope);
                success = wrapper_index >= 0;

                if (success) {
                    add_run_task(inst, task.node, task.scope, task.insert_bc_deps, wrapper_index);
                }

            } else if (task.kind == SSA_Task_Kind::GLOBAL_VAR) {
                success = ssa_emit_global_variable(inst, inst->ssa_program, task.node.declaration);
                assert(success);

            } else if (task.kind == SSA_Task_Kind::CONSTANT) {

                u32 const_index = ssa_emit_constant(inst, inst->ssa_program, task.node.declaration->constant.value);
                darray_append(&inst->ssa_program->constant_references, { task.node, const_index });
                success = true;

            } else {
                assert(task.kind == SSA_Task_Kind::FUNCTION);
                success = ssa_emit_function(inst, inst->ssa_program, task.node.declaration);
                assert(success);
            }

            if (success) {
                progress = true;
                darray_remove_unordered(&inst->ssa_tasks, i);
                i--;

                if (task.bytecode_deps && task.kind != SSA_Task_Kind::CONSTANT) {
                    darray_free(task.bytecode_deps);
                    free(c_allocator(), task.bytecode_deps);
                }
            }
        }


        for (s64 i = 0; i < inst->run_tasks.count; i++) {
            Run_Task* task = &inst->run_tasks[i];

            VM_Result run_result = vm_run(&inst->vm, inst->ssa_program, task->wrapper_index);
            assert(!run_result.assert_fail);

            if (task->node.kind == AST_Node_Kind::EXPRESSION) {
                assert(task->kind == Run_Task_Kind::RUN);
                assert(!task->node.expression->run.generated_expression);

                AST_Expression* gen_expr = vm_const_expr_from_result(inst, run_result);
                assert(gen_expr);

                task->node.expression->run.generated_expression = gen_expr;

                Resolve_Task resolve_task = resolve_task_create(inst, ast_node(gen_expr), task->scope, nullptr, nullptr);
                bool resolve_result = resolve_node(inst, &resolve_task, &resolve_task.node, resolve_task.scope);
                assert(resolve_result);

                Type_Task type_task = type_task_create(inst, ast_node(gen_expr), run_result.type, task->scope, nullptr, nullptr);
                bool type_result = type_node(inst, &type_task, &type_task.node, type_task.scope);
                assert(type_result);

                assert(gen_expr->resolved_type == run_result.type);
                assert(gen_expr->flags & AST_EXPR_FLAG_CONST);

            } else {
                assert(task->node.kind == AST_Node_Kind::STATEMENT);

                if (task->kind == Run_Task_Kind::INSERT) {

                    auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);
                    String insert_string = vm_string_from_result(inst, &inst->temp_allocator, run_result);

                    // TODO: dynamic allocator
                    insert_string = fix_special_characters_in_insert_string(inst, c_allocator(), insert_string);
                    temp_allocator_reset(&inst->temp_allocator_data, mark);

                    Source_Pos pos = source_pos(inst, task->node);

                    u32 offset = add_insert_string(inst, pos, insert_string);
                    assert(offset >= 0);

                    assert(task->node.statement->kind == AST_Statement_Kind::INSERT);
                    AST_Declaration* fn_decl = task->node.statement->insert.fn_decl;

                    add_parse_task(inst, atom_get(inst->inserted_strings_path), insert_string, task->scope, fn_decl, task->insert_bc_deps, task->node.statement,  inst->insert_file_index, offset);
                }
            }

            progress = true;
            darray_remove_unordered(&inst->run_tasks, i);
            i--;
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

        if (error_reported) {
            return false;
        }

        for (s64 i = 0; i < inst->type_tasks.count; i++) {
            auto t = &inst->type_tasks[i];

            if (t->waiting_for) {
                assert(t->waiting_for);
                String name = atom_string(t->waiting_for->atom);
                Source_Pos pos = source_pos(inst, t->waiting_for);
                instance_error(inst, pos, "Waiting for untyped declaration: '%s'", name.data);
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

    if (!builtin_module && inst->ssa_program->entry_fn_index >= 0) {
        inst->entry_run_result = vm_run(&inst->vm, inst->ssa_program);
        if (inst->entry_run_result.assert_fail) {
            log_warn("Bytecode vm quit after failed assert");
        }
        log_debug("Bytecode vm returned: %llu", inst->entry_run_result.return_value);
    }

    if (builtin_module) {
        return true;
    } else {
        return backend_emit(inst);
    }
}

u32 add_insert_string(Instance* inst, Source_Pos insert_pos, String_Ref str)
{
    if (inst->insert_file_index == -1) {
        s64 index = inst->imported_files.count;

        Imported_File insert_file = {
            .name = atom_get(inst->inserted_strings_path),
            .ast = nullptr,
        };

        // TODO: Dynamic allocator
        darray_init(c_allocator(), &insert_file.newline_offsets);

        darray_append(&inst->imported_files, insert_file);
        inst->insert_file_index = index;
    }

    File_Handle fhandle;
    fs_open(inst->inserted_strings_path, FILE_MODE_WRITE, &fhandle);

    u64 offset_offset;
    fs_size(&fhandle, &offset_offset);

    auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);

    Imported_File inserted_from_file = inst->imported_files[insert_pos.file_index];
    Line_Info insert_li = line_info(inserted_from_file.newline_offsets, insert_pos.offset);

    String comment_string = string_format(&inst->temp_allocator, "// Inserted from: %s:%u:%u\n",
                                          atom_string(inserted_from_file.name).data, insert_li.line, insert_li.offset);


    Imported_File* insert_file = &inst->imported_files[inst->insert_file_index];

    fs_append(&fhandle, comment_string);
    offset_offset += comment_string.length;

    temp_allocator_reset(&inst->temp_allocator_data, mark);

    u32 result = offset_offset;

    darray_append(&insert_file->newline_offsets, result - 1);

    fs_append(&fhandle, str);
    offset_offset += str.length;

    fs_append(&fhandle, "\n\n");
    darray_append(&insert_file->newline_offsets, (u32)offset_offset);
    darray_append(&insert_file->newline_offsets, (u32)offset_offset + 1);

    fs_close(&fhandle);

    return result;
}

static void instance_error_va(Instance* inst, Source_Pos pos, const char* fmt, va_list args)
{
    inst->fatal_error = true;

    Imported_File file = inst->imported_files[pos.file_index];
    String_Ref name = atom_string(file.name);

    Line_Info li = line_info(file.newline_offsets, pos.offset);

    fprintf(stderr, "%s:%d:%d: error: ", name.data, li.line, li.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

static void instance_error_note_va(Instance* inst, Source_Pos pos, const char* fmt, va_list args)
{
    inst->fatal_error = true;

    Imported_File file = inst->imported_files[pos.file_index];
    String_Ref name = atom_string(file.name);

    Line_Info li = line_info(file.newline_offsets, pos.offset);

    fprintf(stderr, "%s:%d:%d: note: ", name.data, li.line, li.offset);
    fprintf(stderr, "note: ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

String fix_special_characters_in_insert_string(Instance* inst, Allocator* allocator, String_Ref str)
{

    int escape_count = 0;
    bool in_string_literal = false;

    for (s64 i = 0; i < str.length; i++) {

        if (str[i] == '"') {
            in_string_literal = !in_string_literal;

        } else if (in_string_literal && is_special_character(str[i]) != -1) {
            escape_count += 1;
        }
    }

    assert(!in_string_literal);

    if (!escape_count) return string_copy(allocator, str);

    s64 new_length = str.length + escape_count;
    String result {
        .data = allocate_array<char>(allocator, new_length + 1),
        .length = new_length,
    };

    s64 insert_idx = 0;
    for (s64 i = 0; i < str.length; i++) {

        char c = str[i];

        if (c == '"') {
            in_string_literal = !in_string_literal;

        } else if (in_string_literal) {
            s64 special_index = is_special_character(c);

            if (special_index != -1) {
                result.data[insert_idx++] = '\\';
                c = get_escape_char(special_index);
            }
        }

        result.data[insert_idx++] = c;
    }

    result.data[result.length] = '\0';
    return result;
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
