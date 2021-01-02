#pragma once

#include "src/defs.hh"
#include "src/ir/ir.hh"
#include "src/overloaded.hh"

#include <ostream>
#include <variant>
#include <vector>

namespace ir {

std::ostream& operator<<(std::ostream& os, const Label& lname);
std::ostream& operator<<(std::ostream& os, const Var& vname);
std::ostream& operator<<(std::ostream& os, const FnName& fname);
std::ostream& operator<<(std::ostream& os, const VTableName& vt_name);
std::ostream& operator<<(std::ostream& os, const VTable& vt);
std::ostream& operator<<(std::ostream& os, const StringConstantName& sname);
std::ostream& operator<<(std::ostream& os, const StringConstant& sc);
std::ostream& operator<<(std::ostream& os, const Null& /*unused*/);

std::ostream& operator<<(std::ostream& os, const Value& val);

std::ostream& operator<<(std::ostream& os, Type t);

std::ostream& operator<<(std::ostream& os, UnaryOp uop);

std::ostream& operator<<(std::ostream& os, BinOp bop);

std::ostream& operator<<(std::ostream& os, RelOp rop);

std::ostream& operator<<(std::ostream& os, const MemLoc& mloc);
std::ostream& operator<<(std::ostream& os, const ConstMemLoc& cmloc);

std::ostream& operator<<(std::ostream& os, const ICopy& i);
std::ostream& operator<<(std::ostream& os, const IUnaryOp& i);
std::ostream& operator<<(std::ostream& os, const IBinOp& i);
std::ostream& operator<<(std::ostream& os, const ILoad& i);
std::ostream& operator<<(std::ostream& os, const IConstLoad& i);
std::ostream& operator<<(std::ostream& os, const IStore& i);
std::ostream& operator<<(std::ostream& os, const std::vector<Value>& vals);
std::ostream& operator<<(std::ostream& os, const ICall& i);
std::ostream& operator<<(std::ostream& os, const IVCall& i);
std::ostream& operator<<(std::ostream& os, const IGoto& i);
std::ostream& operator<<(std::ostream& os, const IIfUnaryCond& i);
std::ostream& operator<<(std::ostream& os, const IIfBinCond& i);
std::ostream& operator<<(std::ostream& os, const IReturn& i);
std::ostream& operator<<(std::ostream& os, const IUnreachable& i);

std::ostream& operator<<(std::ostream& os, const Instruction& instr);

std::ostream& operator<<(std::ostream& os, const Phi& phi);

std::ostream& operator<<(std::ostream& os, const BasicBlock& block);

std::ostream& operator<<(std::ostream& os, const FnArg& farg);

std::ostream& operator<<(std::ostream& os, const std::vector<FnArg>& fargs);

std::ostream& operator<<(std::ostream& os, const FnDef& fdef);

std::ostream& operator<<(std::ostream& os, const Program& prog);

} // namespace ir
