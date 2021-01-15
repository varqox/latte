#pragma once

#include "src/ir/ir.hh"

#include <vector>

namespace ir {

[[nodiscard]] std::vector<BasicBlock>
global_subexpression_elimination(std::vector<BasicBlock>&& body);

} // namespace ir
