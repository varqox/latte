#pragma once

#include "src/ir/ir.hh"

namespace ir {

[[nodiscard]] Program make_ssa(Program&& prog);

} // namespace ir
