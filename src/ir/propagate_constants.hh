#pragma once

#include "src/ir/ir.hh"

#include <vector>

namespace ir {

[[nodiscard]] std::vector<BasicBlock> propagate_constants(std::vector<BasicBlock>&& body);

} // namespace ir
