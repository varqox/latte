#pragma once

#include "src/ir/ir.hh"

#include <map>
#include <set>
#include <vector>

namespace ir {

struct BBInfo {
    ir::BasicBlock* bblock;
    std::set<ir::Label> predecessors;
    std::vector<ir::Label> successors;
};

std::map<Label, BBInfo> bblocks_pred_succ_info(std::vector<BasicBlock>& body);

} // namespace ir
