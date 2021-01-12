#pragma once

#include "src/ir/ir.hh"

namespace ir {

[[nodiscard]] Program optimize(Program&& prog);

} // namespace ir
