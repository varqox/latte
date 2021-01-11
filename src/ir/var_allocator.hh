#pragma once

#include "src/ir/ir.hh"
#include "src/ir/name_allocator.hh"

namespace ir {

class VarAllocator {
    NameAllocator<VarName> var_name_allocator;

public:
    constexpr VarAllocator() noexcept = default;

    constexpr explicit VarAllocator(VarName largest_name_in_use) noexcept
    : var_name_allocator{largest_name_in_use} {}

    Var alloc(Type var_type) noexcept {
        return {
            .name = var_name_allocator.alloc(),
            .type = var_type,
        };
    }
};

} // namespace ir
