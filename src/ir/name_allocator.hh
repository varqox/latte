#pragma once

#include <cstddef>
#include <type_traits>

template <class XName>
class NameAllocator {
    static_assert(std::is_same_v<decltype(XName::id), size_t>);
    size_t next_id = 0;

public:
    constexpr NameAllocator() noexcept = default;

    constexpr explicit NameAllocator(XName largest_name_in_use) noexcept
    : next_id{largest_name_in_use.id + 1} {}

    XName alloc() noexcept { return XName{.id = next_id++}; }
};
