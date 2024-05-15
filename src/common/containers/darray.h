#pragma once

#include "defines.h"
#include "memory/allocator.h"
#include "memory/temp_allocator.h"

#include <cassert>
#include <cstring>

namespace Novo {

#define NOVO_DARRAY_DEFAULT_CAPACITY 8
#define NOVO_QUICKSORT_INSERT_THRESHOLD 16

template <typename Element_Type>
struct DArray
{
    Element_Type* data = nullptr;
    s64 count = 0;
    s64 capacity = 0;
    Allocator* backing_allocator = nullptr;

    Element_Type& operator[](s64 index) {
        assert(index >= 0 && index < count);
        return data[index];
    }

    const Element_Type& operator[](s64 index) const {
        assert(index >= 0 && index < count);
        return data[index];
    }
};

template <typename T>
struct Temp_Array;

template <typename Element_Type>
struct Array_Ref
{
    Element_Type* data = nullptr;
    s64 count = 0;

    Array_Ref() = default;

    Array_Ref(const DArray<Element_Type>& dyn_arr) : data(dyn_arr.data), count(dyn_arr.count) { }
    Array_Ref(const Temp_Array<Element_Type>& temp_arr) : data(temp_arr.array.data), count(temp_arr.array.count) { }
    Array_Ref(Element_Type* data, s64 count) : data(data), count(count) { }

    template <std::size_t N>
    constexpr Array_Ref(const Element_Type (&c_arr)[N]) : data((Element_Type*)c_arr), count(N) {}

    Array_Ref(const Element_Type* begin, const Element_Type* end) : data(begin), count(end - begin) {}

    Element_Type& operator[](s64 index) {
        assert(index >= 0 && index < count);
        return data[index];
    }

    const Element_Type& operator[](s64 index) const {
        assert(index >= 0 && index < count);
        return data[index];
    }
};

template <typename Element_Type>
void darray_init(Allocator* backing_allocator, DArray<Element_Type>* out_array, s64 capacity = NOVO_DARRAY_DEFAULT_CAPACITY)
{
    assert(backing_allocator && out_array);
    assert(capacity >= 0);

    if (capacity) out_array->data = allocate_array<Element_Type>(backing_allocator, capacity);
    else out_array->data = nullptr;

    out_array->count = 0;
    out_array->capacity = capacity;
    out_array->backing_allocator = backing_allocator;
}

template <typename Element_Type>
void darray_free(DArray<Element_Type>* array)
{
    if (array->data) {
        assert(array->capacity);
        if (!(array->backing_allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
            free(array->backing_allocator, array->data);
        }
    }

    *array = {};
}

template <typename Element_Type>
void darray_grow(DArray<Element_Type>* array, s64 new_cap)
{
    assert(new_cap > array->capacity);

    Element_Type* new_data = allocate_array<Element_Type>(array->backing_allocator, new_cap);
    assert(new_data);
    if (array->capacity) {
        memcpy(new_data, array->data, sizeof(Element_Type) * array->count);

        assert(array->data);
        if (!(array->backing_allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
            free(array->backing_allocator, array->data);
        }
    }
    array->data = new_data;
    array->capacity = new_cap;
}

template <typename Element_Type>
void darray_reserve(DArray<Element_Type>*array, s64 capacity)
{
    if (array->capacity < capacity) {
        darray_grow(array, capacity);
    }
}

template <typename Element_Type>
Element_Type* darray_append(DArray<Element_Type>* array, Element_Type element)
{
    if (array->count >= array->capacity) {
        s64 new_cap = max(array->capacity * 2, (s64)1);
        assert(new_cap);

        darray_grow(array, new_cap);
        assert(array->capacity > array->count);
    }

    auto index = array->count;

    array->data[index] = element;
    array->count += 1;

    return &array->data[index];
}

template <typename Element_Type>
void darray_append_array(DArray<Element_Type>* array, const Array_Ref<Element_Type>& elements)
{
    darray_reserve(array, array->count + elements.count);

    memcpy(&array->data[array->count], elements.data, elements.count * sizeof(Element_Type));
    array->count += elements.count;
    assert(array->count <= array->capacity);
}

template <typename Element_Type>
Element_Type* darray_append_unique(DArray<Element_Type>* array, Element_Type element) {

    for (s64 i = 0; i < array->count; i++) {
        if (array->data[i] == element) {
            return &array->data[i];
        }
    }

    return darray_append(array, element);
}

template <typename Element_Type>
Element_Type* darray_insert(DArray<Element_Type>* array, Element_Type element, s64 index = 0)
{
    assert(index >= 0 && index <= array->count);

    if (array->count >= array->capacity) {
        darray_grow(array);
    }

    auto copy_size = sizeof(Element_Type) * (array->count - index);
    memmove(&array->data[index + 1], &array->data[index], copy_size);

    array->data[index] = element;
    array->count += 1;

    return &array->data[index];
}

template <typename Element_Type>
DArray<Element_Type> darray_copy(Allocator* allocator, const Array_Ref<Element_Type>& source)
{
    if (source.count == 0) return {};

    DArray<Element_Type> result;
    darray_init(allocator, &result, source.count);

    memcpy(result.data, source.data, sizeof(Element_Type) * source.count);
    result.count = source.count;

    return result;
}

template <typename Element_Type>
DArray<Element_Type> darray_copy(Allocator* allocator, DArray<Element_Type>* source)
{
    return darray_copy(allocator, Array_Ref<Element_Type>(*source));
}

template <typename Element_Type>
void darray_remove_unordered(DArray<Element_Type>* array, s64 index)
{
    assert(array);
    assert(index >= 0);
    assert(array->count > index);

    array->data[index] = array->data[array->count - 1];
    array->count -= 1;
}

template <typename Element_Type>
void darray_remove_ordered(DArray< Element_Type>* array, s64 index)
{
    assert(array && array->data && array->count);
    assert(index >= 0 && index <= array->count);

    if (index == array->count - 1) {
        array->count -= 1;
        return;
    }

    auto copy_count = array->count - 1 - index;
    auto copy_size = sizeof(Element_Type) * copy_count;

    memmove(&array->data[index], &array->data[index + 1], copy_size);

    array->count -= 1;
}

template <typename T>
struct Temp_Array
{
    Temp_Allocator_Mark mark;
    DArray<T> array;

    T& operator[](s64 index) {
        return array.operator[](index);
    }

    const T& operator[](s64 index) const {
        return array.operator[](index);
    }
};

template <typename T>
static Temp_Array<T> temp_array_create(Allocator* allocator, s64 cap = 0)
{
    Temp_Array<T> result;

    auto tas = (Temp_Allocator*)allocator->user_data;
    assert(tas);

    result.mark = temp_allocator_get_mark(tas);
    darray_init(allocator, &result.array, cap);
    return result;
}

template <typename T>
static void temp_array_destroy(Temp_Array<T>* ta)
{
    if (ta->array.backing_allocator) {
        auto tas = (Temp_Allocator*)ta->array.backing_allocator->user_data;
        assert(tas);

        temp_allocator_reset(tas, ta->mark);
    }
}

template <typename T>
static DArray<T> temp_array_finalize(Allocator* allocator, Temp_Array<T>* ta)
{
    auto result = darray_copy(allocator, &ta->array);
    temp_array_destroy(ta);
    return result;
}

template <typename T>
static void darray_append(Temp_Array<T>* ta, T element) {
    darray_append(&ta->array, element);
}

#define QS_SWAP(a, b) { \
    T t = (a); \
    (a) = (b); \
    (b) = t; \
}

template <typename T>
NINLINE int compare(T a, T b) {
    return (int)(a - b);
}

template <typename T>
using Compare_FN_Type = int(*)(T, T);

template <typename T>
static void pivot_sort(Array_Ref<T> ta, s64 lo, s64 hi, Compare_FN_Type<T> compare_fn) {

    s64 mi = (lo + hi) / 2;

    if (compare_fn(ta[mi], ta[lo]) < 0) {
        QS_SWAP(ta[mi], ta[lo]);
    }
    if (compare_fn(ta[hi], ta[lo]) < 0) {
        QS_SWAP(ta[hi], ta[lo]);
    }
    if (compare_fn(ta[mi], ta[hi]) < 0) {
        QS_SWAP(ta[mi], ta[hi]);
    }

    T pivot = ta[hi];

    s64 left_index = lo;
    s64 right_index = hi - 1;

    do {

        for (; left_index < hi; left_index++) {
            if (compare_fn(ta[left_index], pivot) > 0) break;
        }

        for (; right_index >= lo; right_index--) {
            if(compare_fn(ta[right_index], pivot) < 0) break;
        }

        if (left_index > right_index) break;

        QS_SWAP(ta[left_index], ta[right_index]);

    } while (true);

    QS_SWAP(ta[left_index], ta[hi]);

    if (left_index > lo + 1) {
        pivot_sort(ta, lo, left_index - 1, compare_fn);
    }

    if (left_index <= hi - 2) {
        pivot_sort(ta, left_index + 1, hi, compare_fn);
    }

}

template <typename T>
static void quicksort(Array_Ref<T> array, Compare_FN_Type<T> compare_fn = compare) {
    if (array.count > NOVO_QUICKSORT_INSERT_THRESHOLD) {
        pivot_sort(array, 0, array.count - 1, compare_fn);
    } else {
        // Insertion sort
        for (s64 i = 1; i < array.count; i++) {
            T t = array[i];
            s64 j = i;
            for (; j > 0 && compare_fn(array[j - 1], t) > 0; j--) {
                 array[j] = array[j - 1];
            }
            array[j] = t;
        }
    }
}

#undef QS_SWAP

}
