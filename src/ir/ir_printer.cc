#include "src/ir/ir_printer.hh"
#include "src/defs.hh"
#include "src/ir/ir.hh"
#include "src/overloaded.hh"

#include <cstddef>
#include <iomanip>
#include <ios>
#include <ostream>
#include <variant>

namespace ir {

std::ostream& operator<<(std::ostream& os, const Label& lname) { return os << to_str(lname); }
std::ostream& operator<<(std::ostream& os, const FnName& fname) { return os << to_str(fname); }
std::ostream& operator<<(std::ostream& os, const VTableName& vt_name) {
    return os << to_str(vt_name);
}
std::ostream& operator<<(std::ostream& os, const VTable& vt) {
    os << vt.name << " [\n";
    for (auto const& method_name : vt.methods) {
        os << "    " << method_name << ",\n";
    }
    return os << ']';
}
std::ostream& operator<<(std::ostream& os, const StringConstantName& sname) {
    return os << to_str(sname);
}
std::ostream& operator<<(std::ostream& os, const StringConstant& sc) {
    return os << sc.name << " = " << std::quoted(sc.value);
}
std::ostream& operator<<(std::ostream& os, const Null& /*unused*/) { return os << "null"; }

std::ostream& operator<<(std::ostream& os, const Value& val) {
    std::visit(
        overloaded{
            [&](const Var& v) { os << v; },
            [&](int_t i) { os << i; },
            [&](bool b) { os << std::boolalpha << b; },
            [&](const Null& n) { os << n; },
            [&](const StringConstantName& sn) { os << sn; },
            [&](const VTableName& vtn) { os << vtn; },
        },
        val);
    return os;
}

std::ostream& operator<<(std::ostream& os, Type t) { return os << to_str(t); }

std::ostream& operator<<(std::ostream& os, const VarName& vname) {
    return os << 'v' << vname.id;
}
std::ostream& operator<<(std::ostream& os, const Var& var) {
    return os << var.type << ' ' << var.name;
}

std::ostream& operator<<(std::ostream& os, UnaryOp uop) {
    switch (uop) {
    case UnaryOp::NEG: return os << "-";
    case UnaryOp::NOT: return os << "!";
    }
    __builtin_unreachable();
}

std::ostream& operator<<(std::ostream& os, BinOp bop) {
    switch (bop) {
    case BinOp::ADD: return os << "+";
    case BinOp::SUB: return os << "-";
    case BinOp::MUL: return os << "*";
    case BinOp::DIV: return os << "/";
    case BinOp::MOD: return os << "%";
    }
    __builtin_unreachable();
}

std::ostream& operator<<(std::ostream& os, RelOp rop) {
    switch (rop) {
    case RelOp::LTH: return os << "<";
    case RelOp::LE: return os << "<=";
    case RelOp::GTH: return os << ">";
    case RelOp::GE: return os << ">=";
    case RelOp::EQ: return os << "==";
    case RelOp::NE: return os << "!=";
    }
    __builtin_unreachable();
}

std::ostream& operator<<(std::ostream& os, const MemLoc& mloc) {
    os << '[';
    std::visit([&](auto const& x) { os << x; }, mloc.base);
    if (mloc.scale != 0) {
        os << " + " << mloc.scale << " * ";
        std::visit([&](auto const& x) { os << x; }, mloc.index);
    }
    if (mloc.displacement != 0) {
        os << " + " << mloc.displacement;
    }
    return os << ']';
}
std::ostream& operator<<(std::ostream& os, const ConstMemLoc& cmloc) {
    return os << "const " << cmloc.loc;
}

std::ostream& operator<<(std::ostream& os, const ICopy& i) {
    return os << i.var << " = " << i.val;
}
std::ostream& operator<<(std::ostream& os, const IUnaryOp& i) {
    return os << i.var << " = " << i.op << i.val;
}
std::ostream& operator<<(std::ostream& os, const IBinOp& i) {
    return os << i.var << " = " << i.left << ' ' << i.op << ' ' << i.right;
}
std::ostream& operator<<(std::ostream& os, const ILoad& i) {
    return os << i.var << " = " << i.loc;
}
std::ostream& operator<<(std::ostream& os, const IConstLoad& i) {
    return os << i.var << " = " << i.loc;
}
std::ostream& operator<<(std::ostream& os, const IStore& i) {
    return os << i.loc << " = " << i.val;
}
std::ostream& operator<<(std::ostream& os, const std::vector<Value>& vals) {
    bool first = true;
    for (auto const& val : vals) {
        if (first) {
            first = false;
        } else {
            os << ", ";
        }
        os << val;
    }
    return os;
}
inline std::ostream& operator<<(std::ostream& os, const decltype(ICall::func)& func) {
    std::visit([&](auto const& x) { os << x; }, func);
    return os;
}
std::ostream& operator<<(std::ostream& os, const ICall& i) {
    return os << i.var << " = " << i.func << '(' << i.args << ')';
}
std::ostream& operator<<(std::ostream& os, const IVCall& i) {
    return os << i.func << '(' << i.args << ')';
}
std::ostream& operator<<(std::ostream& os, const IGoto& i) {
    return os << "goto " << i.target;
}
std::ostream& operator<<(std::ostream& os, const IIfUnaryCond& i) {
    return os << "if " << (i.negate_cond ? "!" : "") << i.cond << " goto " << i.true_branch
              << " else " << i.false_branch;
}
std::ostream& operator<<(std::ostream& os, const IIfBinCond& i) {
    return os << "if " << i.left << ' ' << i.op << ' ' << i.right << " goto " << i.true_branch
              << " else " << i.false_branch;
}
std::ostream& operator<<(std::ostream& os, const IReturn& i) {
    os << "ret";
    if (i.val) {
        os << ' ' << *i.val;
    }
    return os;
}
std::ostream& operator<<(std::ostream& os, const IUnreachable& /*unused*/) {
    return os << "unreachable";
}

std::ostream& operator<<(std::ostream& os, const Instruction& instr) {
    std::visit([&](auto const& i) { os << i; }, instr);
    return os;
}

std::ostream& operator<<(std::ostream& os, const BasicBlock& block) {
    os << ' ' << block.name << ":\n";
    for (auto const& [var, predecessors] : block.phis) {
        os << "    " << var << " = phi [";
        bool first = true;
        for (auto const& [label, val] : predecessors) {
            if (first) {
                first = false;
            } else {
                os << ", ";
            }
            os << label << ": " << val;
        }
        os << "]\n";
    }
    for (auto const& instr : block.instructions) {
        os << "    " << instr << '\n';
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const std::vector<Var>& fargs) {
    bool first = true;
    for (auto const& arg : fargs) {
        if (first) {
            first = false;
        } else {
            os << ", ";
        }
        os << arg;
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const FnDef& fdef) {
    os << fdef.name << '(' << fdef.args << ')';
    if (fdef.ret_type) {
        os << " -> " << *fdef.ret_type;
    }
    os << " {\n";
    for (auto const& block : fdef.body) {
        os << block;
    }
    return os << "}\n";
}

std::ostream& operator<<(std::ostream& os, const Program& prog) {
    for (auto const& sc : prog.strings) {
        os << sc << '\n';
    }
    for (auto const& vt : prog.vtables) {
        os << vt << '\n';
    }
    for (auto const& fn : prog.functions) {
        os << '\n' << fn;
    }
    return os;
}

} // namespace ir
