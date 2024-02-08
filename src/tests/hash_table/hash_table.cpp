
#include <containers/hash_table.h>
#include <defines.h>
#include <memory/allocator.h>

#include <assert.h>
#include <nstring.h>

using namespace Novo;

static bool int_eq_fn(int a, int b) {
    return a == b;
}

static void create_and_free()
{
    // Default key compare
    {
        Hash_Table<int, const char *> table;
        hash_table_create(c_allocator(), &table);

        assert(table.hashes);
        assert(table.keys);
        assert(table.values);

        assert(table.capacity == NOVO_HASH_TABLE_INITIAL_CAPACITY);

        assert((void *)table.keys_equal == (void *)default_hash_table_equal<int>);
        assert(table.allocator == c_allocator());

        hash_table_free(&table);

        assert(table.hashes == nullptr);
        assert(table.keys == nullptr);
        assert(table.values == nullptr);

        assert(table.capacity == 0);

        assert((void *)table.keys_equal == nullptr);
        assert(table.allocator == nullptr);
    }

    // Custom key compare
    {
        Hash_Table<int, const char *> table;
        hash_table_create(c_allocator(), &table, int_eq_fn);

        assert(table.hashes);
        assert(table.keys);
        assert(table.values);

        assert(table.capacity == NOVO_HASH_TABLE_INITIAL_CAPACITY);

        assert((void *)table.keys_equal == (void *)int_eq_fn);
        assert(table.allocator == c_allocator());

        hash_table_free(&table);

        assert(table.hashes == nullptr);
        assert(table.keys == nullptr);
        assert(table.values == nullptr);

        assert(table.capacity == 0);

        assert((void *)table.keys_equal == nullptr);
        assert(table.allocator == nullptr);
    }
}

static void add_and_find()
{
    Hash_Table<int, const char *> table;
    hash_table_create(c_allocator(), &table);

    auto s1 = "Really?";
    hash_table_add(&table, 42, s1);
    assert(hash_table_count(&table) == 1);

    auto s2 = "Yea!";
    hash_table_add(&table, 43, s2);
    assert(hash_table_count(&table) == 2);

    const char *val_1;
    bool find_res = hash_table_find(&table, 42, &val_1);
    assert(find_res);
    assert(s1 == val_1);
    assert(string_equal(val_1, "Really?"));

    const char *val_2;
    find_res = hash_table_find(&table, 43, &val_2);
    assert(find_res);
    assert(s2 == val_2);
    assert(string_equal(val_2, "Yea!"));

    hash_table_free(&table);
}

static void grow()
{
    Hash_Table<int, int> table;
    hash_table_create(c_allocator(), &table);

    assert(table.capacity == NOVO_HASH_TABLE_INITIAL_CAPACITY);

    for (int i = 0; i < NOVO_HASH_TABLE_INITIAL_CAPACITY; i++) {
        hash_table_add(&table, i, i);
    }

    assert(table.capacity == NOVO_HASH_TABLE_INITIAL_CAPACITY);
    assert(hash_table_count(&table) == NOVO_HASH_TABLE_INITIAL_CAPACITY);

    for (int i = 0; i < NOVO_HASH_TABLE_INITIAL_CAPACITY; i++) {
        hash_table_add(&table, i, i);
    }

    assert(table.capacity == NOVO_HASH_TABLE_INITIAL_CAPACITY * 2);
    assert(hash_table_count(&table) == NOVO_HASH_TABLE_INITIAL_CAPACITY * 2);

    for (int i = 0; i < NOVO_HASH_TABLE_INITIAL_CAPACITY; i++) {
        hash_table_add(&table, i * 3, i * 4);
    }

    assert(table.capacity == NOVO_HASH_TABLE_INITIAL_CAPACITY * 4);
    assert(hash_table_count(&table) == NOVO_HASH_TABLE_INITIAL_CAPACITY * 3);

    hash_table_free(&table);
}

int main(int argc, char* argv[]) {
    create_and_free();
    add_and_find();
    grow();
    return 0;
}
