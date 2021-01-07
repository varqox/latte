#pragma once

#include "src/concat.hh"
#include "src/define_cmp_operators_by_field.hh"
#include "src/defs.hh"
#include "src/overloaded.hh"

#include <cstddef>
#include <map>
#include <optional>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

namespace ir {

struct Label {
    size_t id;
    DEFINE_CMP_OPERATORS_BY_FIELD(Label, id)
};

inline std::string to_str(const Label& label) { return concat(".L", label.id); }

struct Var {
    size_t id;
    DEFINE_CMP_OPERATORS_BY_FIELD(Var, id)
};

struct ClassName {
    std::string mangled_name;
    DEFINE_CMP_OPERATORS_BY_FIELD(ClassName, mangled_name)
};

struct FnName {
    std::string mangled_name;
    DEFINE_CMP_OPERATORS_BY_FIELD(FnName, mangled_name)
};

inline std::string to_str(const FnName& fname) { return concat("f.", fname.mangled_name); }

inline const FnName builtin_zalloc = FnName{.mangled_name = "@@zalloc"}; // (INT) -> PTR
inline const FnName builtin_free = FnName{.mangled_name = "@@free"}; // (PTR) -> void
inline const FnName builtin_make_string =
    FnName{.mangled_name = "@@make_string"}; // (PTR) -> PTR
inline const FnName builtin_concat_strings =
    FnName{.mangled_name = "@@concat_strings"}; // (PTR, PTR) -> PTR
inline const FnName builtin_strcmp_lth =
    FnName{.mangled_name = "@@strcmp_lth"}; // (PTR, PTR) -> BOOL
inline const FnName builtin_strcmp_le =
    FnName{.mangled_name = "@@strcmp_le"}; // (PTR, PTR) -> BOOL
inline const FnName builtin_strcmp_gth =
    FnName{.mangled_name = "@@strcmp_gth"}; // (PTR, PTR) -> BOOL
inline const FnName builtin_strcmp_ge =
    FnName{.mangled_name = "@@strcmp_ge"}; // (PTR, PTR) -> BOOL
inline const FnName builtin_strcmp_eq =
    FnName{.mangled_name = "@@strcmp_eq"}; // (PTR, PTR) -> BOOL
inline const FnName builtin_strcmp_ne =
    FnName{.mangled_name = "@@strcmp_ne"}; // (PTR, PTR) -> BOOL
inline const FnName builtin_destruct_string =
    FnName{.mangled_name = "@@destruct_string"}; // (PTR) -> void
inline const FnName builtin_inc_ref_count =
    FnName{.mangled_name = "@@inc_ref_count"}; // (PTR) -> void
inline const FnName builtin_error = FnName{.mangled_name = "@error"}; // () -> void
inline const FnName builtin_printInt = FnName{.mangled_name = "@printInt"}; // (INT) -> void
inline const FnName builtin_printString =
    FnName{.mangled_name = "@printString"}; // (PTR) -> void
inline const FnName builtin_readInt = FnName{.mangled_name = "@readInt"}; // () -> INT
inline const FnName builtin_readString = FnName{.mangled_name = "@readString"}; // () -> INT

struct VTableName {
    ClassName its_class;
    DEFINE_CMP_OPERATORS_BY_FIELD(VTableName, its_class)
};

inline std::string to_str(const VTableName& vt_name) {
    return concat("vt.", vt_name.its_class.mangled_name);
}

struct VTable {
    VTableName name;
    std::vector<FnName> methods;
};

struct StringConstantName {
    size_t id;
    DEFINE_CMP_OPERATORS_BY_FIELD(StringConstantName, id)
};

inline std::string to_str(const StringConstantName& sname) { return concat("s.", sname.id); }

struct StringConstant {
    StringConstantName name;
    std::string value;
};

struct Null {
    friend constexpr bool operator==(Null /*a*/, Null /*b*/) noexcept { return true; }
    friend constexpr bool operator!=(Null /*a*/, Null /*b*/) noexcept { return false; }
    friend constexpr bool operator<(Null /*a*/, Null /*b*/) noexcept { return false; }
    friend constexpr bool operator<=(Null /*a*/, Null /*b*/) noexcept { return true; }
    friend constexpr bool operator>(Null /*a*/, Null /*b*/) noexcept { return false; }
    friend constexpr bool operator>=(Null /*a*/, Null /*b*/) noexcept { return true; }
};

using Value = std::variant<Var, int_t, bool, Null, StringConstantName, VTableName>;

enum class Type {
    BOOL,
    INT,
    PTR,
};

constexpr const char* to_str(Type type) noexcept {
    switch (type) {
    case ir::Type::BOOL: return "BOOL";
    case ir::Type::INT: return "INT";
    case ir::Type::PTR: return "PTR";
    }
    __builtin_unreachable();
}

enum class UnaryOp {
    NEG,
    NOT,
};

enum class BinOp {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
};

enum class RelOp {
    LTH,
    LE,
    GTH,
    GE,
    EQ,
    NE,
};

struct MemLoc {
    std::variant<Var, Null> base;
    size_t displacement;
    size_t scale;
    std::variant<Var, int_t> index;
};

struct ConstMemLoc {
    MemLoc loc;
};

struct ICopy {
    Var var;
    Type type;
    Value val;
};
struct IUnaryOp {
    Var var;
    Type type;
    UnaryOp op;
    Value val;
};
struct IBinOp {
    Var var;
    Type type;
    BinOp op;
    Value left;
    Value right;
};
struct ILoad {
    Var var;
    Type type;
    MemLoc loc;
};
// Just like ILoad, but it is guaranteed that the value under loc is always read-only
struct IConstLoad {
    Var var;
    Type type;
    ConstMemLoc loc;
};
struct IStore {
    MemLoc loc;
    Type type;
    Value val;
};
struct CallArg {
    Value val;
    Type type;
};
struct ICall {
    Var var;
    Type type;
    std::variant<FnName, ConstMemLoc> func;
    std::vector<CallArg> args;
};
struct IVCall {
    std::variant<FnName, ConstMemLoc> func;
    std::vector<CallArg> args;
};
struct IGoto {
    Label target;
};
struct IIfUnaryCond {
    bool negate_cond;
    Var cond;
    Label true_branch;
    Label false_branch;
};
struct IIfBinCond {
    RelOp op;
    Type type;
    Value left;
    Value right;
    Label true_branch;
    Label false_branch;
};
struct IReturn {
    std::optional<Value> val;
};
struct IUnreachable {};

using Instruction = std::variant<
    ICopy, IUnaryOp, IBinOp, ILoad, IConstLoad, IStore, ICall, IVCall, IGoto, IIfUnaryCond,
    IIfBinCond, IReturn, IUnreachable>;

[[nodiscard]] constexpr bool ends_bblock(const Instruction& i) noexcept {
    return std::visit(
        overloaded{
            [](const ICopy& /*unused*/) { return false; },
            [](const IUnaryOp& /*unused*/) { return false; },
            [](const IBinOp& /*unused*/) { return false; },
            [](const ILoad& /*unused*/) { return false; },
            [](const IConstLoad& /*unused*/) { return false; },
            [](const IStore& /*unused*/) { return false; },
            [](const ICall& /*unused*/) { return false; },
            [](const IVCall& /*unused*/) { return false; },
            [](const IGoto& /*unused*/) { return true; },
            [](const IIfUnaryCond& /*unused*/) { return true; },
            [](const IIfBinCond& /*unused*/) { return true; },
            [](const IReturn& /*unused*/) { return true; },
            [](const IUnreachable& /*unused*/) { return true; },
        },
        i);
}

struct Phi {
    Var var;
    Type type;
    std::map<Label, Value> predecessors;
};

struct BasicBlock {
    Label name;
    std::vector<Phi> phis;
    std::vector<Instruction> instructions;
};

struct FnArg {
    Var var;
    Type type;
};

struct FnDef {
    FnName name;
    std::optional<Type> ret_type;
    std::vector<FnArg> args;
    std::vector<BasicBlock> body;
};

struct Program {
    std::vector<StringConstant> strings;
    std::vector<VTable> vtables;
    std::vector<FnDef> functions;
};

} // namespace ir