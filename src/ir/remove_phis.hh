#pragma once

#include "src/ir/ir.hh"

namespace ir {

[[nodiscard]] FnDef remove_phis(FnDef&& fdef);

} // namespace ir
