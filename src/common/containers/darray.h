#pragma once

#include "defines.h"
#include "memory/allocator.h"
#include "memory/temp_allocator.h"

#include <cassert>
#include <cstring>

namespace Novo {

#define NOVO_DARRAY_DEFAULT_CAPACITY 8

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
DArray<Element_Type> darray_copy(const Array_Ref<Element_Type>& source, Allocator* allocator)
{
    if (source.count == 0) return {};

    DArray<Element_Type> result;
    darray_init(allocator, &result, source.count);

    memcpy(result.data, source.data, sizeof(Element_Type) * source.count);
    result.count = source.count;

    return result;
}

template <typename Element_Type>
DArray<Element_Type> darray_copy(DArray<Element_Type>* source, Allocator* allocator)
{
    return darray_copy(Array_Ref<Element_Type>(*source), allocator);
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
    auto tas = (Temp_Allocator*)ta->array.backing_allocator->user_data;
    assert(tas);

    temp_allocator_reset(tas, ta->mark);
}

template <typename T>
static DArray<T> temp_array_finalize(Allocator* allocator, Temp_Array<T>* ta)
{
    auto result = darray_copy(&ta->array, allocator);
    temp_array_destroy(ta);
    return result;
}

template <typename T>
static void darray_append(Temp_Array<T>* ta, T element) {
    darray_append(&ta->array, element);
}

#define QS_SWAP(a, b) { \
    T t = a; \
    a = b; \
    b = t; \
}

template <typename T>
static void quicksort1(Array_Ref<T> &ta, s64 lo, s64 hi) {

    s64 len = (hi + 1) - lo;
    if (len == 2) {
        if (ta[lo] > ta[hi]) {
            QS_SWAP(ta[lo], ta[hi]);
        }
        return;
    }

    s64 pivot_index = (lo + hi) / 2;
    T pivot = ta[pivot_index];

    QS_SWAP(ta[pivot_index], ta[hi]);

    s64 left_index = lo;
    s64 right_index = hi - 1;

    do {

        for (; left_index < hi; left_index++) {
            if (ta[left_index] > pivot) break;
        }

        for (; right_index >= lo; right_index--) {
            if(ta[right_index] < pivot) break;
        }

        if (left_index > right_index) break;

        QS_SWAP(ta[left_index], ta[right_index]);

    } while (true);

    QS_SWAP(ta[left_index], ta[hi]);

    if (left_index > lo + 1) {
        quicksort1(ta, lo, left_index - 1);
    }

    if (left_index <= hi - 2) {
        quicksort1(ta, left_index + 1, hi);
    }

}

template <typename T>
static void quicksort1(Array_Ref<T> &ta) {
    quicksort1(ta, 0, ta.count - 1);
}

template <typename T>
static void quicksort2(Array_Ref<T> &ta, s64 lo, s64 hi) {

    s64 len = (hi + 1) - lo;
    if (len == 2) {
        if (ta[lo] > ta[hi]) {
            QS_SWAP(ta[lo], ta[hi]);
        }
        return;
    }

    s64 mi = (lo + hi) / 2;

    if (ta[mi] < ta[lo]) {
        QS_SWAP(ta[mi], ta[lo]);
    }
    if (ta[hi] < ta[lo]) {
        QS_SWAP(ta[hi], ta[lo]);
    }
    if (ta[mi] < ta[hi]) {
        QS_SWAP(ta[mi], ta[hi]);
    }

    T pivot = ta[hi];

    s64 left_index = lo;
    s64 right_index = hi - 1;

    do {

        for (; left_index < hi; left_index++) {
            if (ta[left_index] > pivot) break;
        }

        for (; right_index >= lo; right_index--) {
            if(ta[right_index] < pivot) break;
        }

        if (left_index > right_index) break;

        QS_SWAP(ta[left_index], ta[right_index]);

    } while (true);

    QS_SWAP(ta[left_index], ta[hi]);

    if (left_index > lo + 1) {
        quicksort2(ta, lo, left_index - 1);
    }

    if (left_index <= hi - 2) {
        quicksort2(ta, left_index + 1, hi);
    }

}

template <typename T>
static void quicksort2(Array_Ref<T> &ta) {
    quicksort2(ta, 0, ta.count - 1);
}

}
