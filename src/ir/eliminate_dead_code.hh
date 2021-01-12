#pragma once

#include "src/ir/ir.hh"

#include <vector>

namespace ir {

[[nodiscard]] std::vector<BasicBlock> eliminate_dead_code(std::vector<BasicBlock>&& body);

} // namespace ir
