#pragma once

#include "src/ir/ir.hh"

#include <fstream>

namespace backend {

void emit_x86_64(ir::Program&& prog, std::ofstream& out, bool disable_destructors);

} // namespace backend
