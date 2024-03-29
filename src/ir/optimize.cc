#include "src/ir/optimize.hh"
#include "src/ir/eliminate_dead_code.hh"
#include "src/ir/eliminate_unnecessary_phis.hh"
#include "src/ir/global_subexpression_elimination.hh"
#include "src/ir/ir.hh"
#include "src/ir/propagate_constants.hh"
#include "src/ir/propagate_copies.hh"

namespace ir {

[[nodiscard]] static FnDef optimize(FnDef&& fdef) {
    while (true) {
        auto new_body = fdef.body;
        new_body = propagate_constants(std::move(new_body));
        new_body = propagate_copies(std::move(new_body));
        new_body = eliminate_unnecessary_phis(std::move(new_body));
        new_body = eliminate_dead_code(std::move(new_body));
        new_body = global_subexpression_elimination(std::move(new_body));
        if (new_body == fdef.body) {
            break;
        }
        fdef.body = std::move(new_body);
    }
    return std::move(fdef);
}

Program optimize(Program&& prog) {
    for (auto& fdef : prog.functions) {
        fdef = optimize(std::move(fdef));
    }
    return std::move(prog);
}

} // namespace ir
