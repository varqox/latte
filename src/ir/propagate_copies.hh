#pragma once

#include "src/ir/ir.hh"

#include <vector>

namespace ir {

[[nodiscard]] std::vector<BasicBlock> propagate_copies(std::vector<BasicBlock>&& body);

} // namespace ir
