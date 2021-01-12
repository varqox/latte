#pragma once

#include "src/ir/ir.hh"

#include <vector>

namespace ir {

[[nodiscard]] std::vector<BasicBlock>
eliminate_unnecessary_phis(std::vector<BasicBlock>&& body);

} // namespace ir
