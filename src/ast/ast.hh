#pragma once

#include "src/concat.hh"
#include "src/defs.hh"
#include "src/overloaded.hh"
#include "src/persistent_map.hh"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace ast {

using Ident = std::string;

struct SrcLoc {
    int line; // starts at 1
    int column; // starts at 1
};

inline std::string as_str(const SrcLoc& sloc) { return concat(sloc.line, ':', sloc.column); }

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
    LTH,
    LE,
    GTH,
    GE,
    EQ,
    NE,
    AND,
    OR,
};

struct Type {
    struct TNull {};
    struct TInt {};
    struct TStr {};
    struct TBool {};
    struct TVoid {};
    struct TArray {
        std::shared_ptr<Type> elem_type;
    };
    struct TClass {
        Ident name;
    };
    struct TFun {
        std::shared_ptr<Type> ret_type;
        std::shared_ptr<std::vector<Type>> arg_types;
    };
    std::variant<TNull, TInt, TStr, TBool, TVoid, TArray, TClass, TFun> val;
    SrcLoc sloc;

    friend bool operator==(const Type& a, const Type& b) noexcept;

    friend std::string as_str(const Type::TFun& tf);
    friend std::string as_str(const Type& t) {
        return std::visit(
            overloaded{
                [](const Type::TNull& /*unused*/) -> std::string { return "null"; },
                [](const Type::TInt& /*unused*/) -> std::string { return "int"; },
                [](const Type::TStr& /*unused*/) -> std::string { return "string"; },
                [](const Type::TBool& /*unused*/) -> std::string { return "boolean"; },
                [](const Type::TVoid& /*unused*/) -> std::string { return "void"; },
                [](const Type::TArray& ta) {
                    auto res = as_str(*ta.elem_type);
                    return res += "[]";
                },
                [](const Type::TClass& tc) { return tc.name; },
                [](const Type::TFun& tf) { return as_str(tf); },
            },
            t.val);
    }

    friend std::string as_str(const Type::TFun& tf) {
        auto res = concat(as_str(*tf.ret_type), " (");
        bool first = true;
        for (auto& arg_type : *tf.arg_types) {
            if (first) {
                first = false;
            } else {
                res += ", ";
            }
            res += as_str(arg_type);
        }
        return res += ')';
    }
};

static inline const Type type_null = {.val = Type::TNull{}, .sloc = {.line = 0, .column = 0}};
static inline const Type type_int = {.val = Type::TInt{}, .sloc = {.line = 0, .column = 0}};
static inline const Type type_bool = {.val = Type::TBool{}, .sloc = {.line = 0, .column = 0}};
static inline const Type type_str = {.val = Type::TStr{}, .sloc = {.line = 0, .column = 0}};
static inline const Type type_void = {.val = Type::TVoid{}, .sloc = {.line = 0, .column = 0}};

inline bool operator==(const Type::TNull& /*a*/, const Type::TNull& /*b*/) noexcept {
    return true;
}
inline bool operator==(const Type::TInt& /*a*/, const Type::TInt& /*b*/) noexcept {
    return true;
}
inline bool operator==(const Type::TStr& /*a*/, const Type::TStr& /*b*/) noexcept {
    return true;
}
inline bool operator==(const Type::TBool& /*a*/, const Type::TBool& /*b*/) noexcept {
    return true;
}
inline bool operator==(const Type::TVoid& /*a*/, const Type::TVoid& /*b*/) noexcept {
    return true;
}
inline bool operator==(const Type::TArray& a, const Type::TArray& b) noexcept {
    return *a.elem_type == *b.elem_type;
}
inline bool operator==(const Type::TClass& a, const Type::TClass& b) noexcept {
    return a.name == b.name;
}
inline bool operator==(const Type::TFun& a, const Type::TFun& b) noexcept {
    return *a.ret_type == *b.ret_type and
        std::equal(
            a.arg_types->begin(), a.arg_types->end(), b.arg_types->begin(),
            b.arg_types->end());
}
inline bool operator==(const Type& a, const Type& b) noexcept { return a.val == b.val; }
inline bool operator!=(const Type::TNull& a, const Type::TNull& b) noexcept {
    return !(a == b);
}
inline bool operator!=(const Type::TInt& a, const Type::TInt& b) noexcept { return !(a == b); }
inline bool operator!=(const Type::TStr& a, const Type::TStr& b) noexcept { return !(a == b); }
inline bool operator!=(const Type::TBool& a, const Type::TBool& b) noexcept {
    return !(a == b);
}
inline bool operator!=(const Type::TVoid& a, const Type::TVoid& b) noexcept {
    return !(a == b);
}
inline bool operator!=(const Type::TArray& a, const Type::TArray& b) noexcept {
    return !(a == b);
}
inline bool operator!=(const Type::TClass& a, const Type::TClass& b) noexcept {
    return !(a == b);
}
inline bool operator!=(const Type::TFun& a, const Type::TFun& b) noexcept { return !(a == b); }
inline bool operator!=(const Type& a, const Type& b) noexcept { return !(a == b); }

inline bool operator<(const Type::TNull& /*a*/, const Type::TNull& /*b*/) noexcept {
    return false;
}
inline bool operator<(const Type::TInt& /*a*/, const Type::TInt& /*b*/) noexcept {
    return false;
}
inline bool operator<(const Type::TStr& /*a*/, const Type::TStr& /*b*/) noexcept {
    return false;
}
inline bool operator<(const Type::TBool& /*a*/, const Type::TBool& /*b*/) noexcept {
    return false;
}
inline bool operator<(const Type::TVoid& /*a*/, const Type::TVoid& /*b*/) noexcept {
    return false;
}
inline bool operator<(const Type& a, const Type& b) noexcept;
inline bool operator<(const Type::TArray& a, const Type::TArray& b) noexcept {
    return *a.elem_type < *b.elem_type;
}
inline bool operator<(const Type::TClass& a, const Type::TClass& b) noexcept {
    return a.name < b.name;
}
inline bool operator<(const Type::TFun& a, const Type::TFun& b) noexcept {
    if (*a.ret_type == *b.ret_type) {
        return std::lexicographical_compare(
            a.arg_types->begin(), a.arg_types->end(), b.arg_types->begin(),
            b.arg_types->end());
    }
    return *a.ret_type < *b.ret_type;
}
inline bool operator<(const Type& a, const Type& b) noexcept { return a.val < b.val; }

struct GlobalSymbols {
    struct Function {
        ast::Type::TFun type;
        ast::SrcLoc def_sloc;
    };
    struct Class {
        const ast::Ident name;
        struct Field {
            const ast::Ident& defined_in_class;
            size_t abs_idx; // field index in the object (including base classes)
            ast::Type type;
            ast::SrcLoc def_sloc;
        };
        struct Method {
            const ast::Ident& defined_in_class;
            size_t vtable_idx;
            ast::Type::TFun type;
            ast::SrcLoc def_sloc;
        };
        std::optional<ast::Ident> base_class;
        PersistentMap<ast::Ident, Field> fields;
        PersistentMap<ast::Ident, Method> methods;
        ast::Type type;
        ast::SrcLoc def_sloc;
        struct {
            size_t in;
            size_t out;
        } pre_post_order;
    };

    static inline const std::map<ast::Ident, ast::Type::TFun> builtin_functions = [] {
        std::map<ast::Ident, ast::Type::TFun> res;
        res.try_emplace(
            "error",
            Type::TFun{
                .ret_type = std::make_unique<Type>(ast::type_void),
                .arg_types = std::make_unique<std::vector<Type>>(),
            });
        res.try_emplace(
            "printInt",
            Type::TFun{
                .ret_type = std::make_unique<Type>(ast::type_void),
                .arg_types = std::make_unique<std::vector<Type>>(1, ast::type_int),
            });
        res.try_emplace(
            "printString",
            Type::TFun{
                .ret_type = std::make_unique<Type>(ast::type_void),
                .arg_types = std::make_unique<std::vector<Type>>(1, ast::type_str),
            });
        res.try_emplace(
            "readInt",
            Type::TFun{
                .ret_type = std::make_unique<Type>(ast::type_int),
                .arg_types = std::make_unique<std::vector<Type>>(),
            });
        res.try_emplace(
            "readString",
            Type::TFun{
                .ret_type = std::make_unique<Type>(ast::type_str),
                .arg_types = std::make_unique<std::vector<Type>>(),
            });
        return res;
    }();
    std::map<ast::Ident, Function> functions;
    std::map<ast::Ident, Class> classes;

    // Returns true iff class_name_a == class_name_b or class_name_a is an ancestor of
    // class_name_b
    bool is_ancestor_of(const ast::Ident& class_name_a, const ast::Ident& class_name_b) const {
        auto a_order = classes.at(class_name_a).pre_post_order;
        auto b_order = classes.at(class_name_b).pre_post_order;
        return a_order.in <= b_order.in and b_order.out <= a_order.out;
    }
};

enum class Reachability {
    REACHABLE, // it seems possible that the whole subtree will be executed (default)
    PARTIAL, // some code, but not all will be executed in the current subtree
    UNREACHABLE, // no code of the current subtree will ever be executed (set from outside
                 // subtree)
};

struct Null {
    friend bool operator==(Null /*a*/, Null /*b*/) noexcept { return true; }
    friend bool operator!=(Null /*a*/, Null /*b*/) noexcept { return false; }
};

struct Expr {
    struct EVar {
        Ident name;
        enum class Kind {
            LOCAL_VAR,
            CLASS_FIELD,
        } kind = Kind::LOCAL_VAR;
        const GlobalSymbols::Class* field_class = nullptr; // set iff it is a field of a class
    };
    struct ELitInt {
        std::string str_val;
    };
    struct ELitBool {
        bool val;
    };
    struct ESelf {};
    struct ENull {};
    struct ECastedNull {
        Type type;
    };
    struct ELitStr {
        std::string val;
    };
    struct EArrElem {
        std::unique_ptr<Expr> arr;
        std::unique_ptr<Expr> index;
        SrcLoc lbracket_sloc;
    };
    struct ECallFunc {
        Ident func_name;
        std::unique_ptr<std::vector<Expr>> args;
        SrcLoc lparen_sloc;
        enum class Kind {
            METHOD,
            BUILTIN,
            FUNCTION,
        } kind = Kind::FUNCTION;
        const GlobalSymbols::Class* method_class = nullptr; // set iff calling a method
    };
    struct EField {
        std::unique_ptr<Expr> object;
        Ident field_name;
        SrcLoc dot_sloc;
        enum class Kind {
            CLASS_FIELD,
            ARRAY_LENGTH,
        } kind = Kind::CLASS_FIELD;
        const GlobalSymbols::Class* field_class =
            nullptr; // set only iff accessing class field
    };
    struct ECallMethod {
        std::unique_ptr<Expr> object;
        Ident method_name;
        std::unique_ptr<std::vector<Expr>> args;
        SrcLoc dot_sloc;
        SrcLoc lparen_sloc;
        const GlobalSymbols::Class* method_class = nullptr;
    };
    struct ENewArray {
        Type elem_type;
        std::unique_ptr<Expr> size;
    };
    struct ENewClass {
        Ident class_name;
    };
    struct EUnaryOp {
        std::unique_ptr<Expr> val;
        UnaryOp op;
        SrcLoc op_sloc;
    };
    struct EBinOp {
        std::unique_ptr<Expr> left;
        std::unique_ptr<Expr> right;
        BinOp op;
        SrcLoc op_sloc;
    };
    std::variant<
        EVar, ELitInt, ELitBool, ESelf, ENull, ECastedNull, ELitStr, EArrElem, ECallFunc,
        EField, ECallMethod, ENewArray, ENewClass, EUnaryOp, EBinOp>
        val;
    SrcLoc sloc;
    Type type = type_null;
    std::optional<std::variant<int_t, bool, std::string, Null>> comptime_val{};
    bool can_be_lvalue = false;
    // It is not propagated down the tree
    Reachability reachability = Reachability::REACHABLE;
};

struct DeclItem {
    struct DNoInit {
        Ident name;
    };
    struct DInit {
        Ident name;
        Expr val;
        SrcLoc ass_sloc;
    };
    std::variant<DNoInit, DInit> val;
    SrcLoc sloc;
    // It is not propagated down the tree
    Reachability reachability = Reachability::REACHABLE;
};

struct Block;

struct Stmt {
    struct SEmpty {};
    struct SBlock {
        std::unique_ptr<Block> block;
    };
    struct SDecl {
        Type type;
        std::vector<DeclItem> items;
    };
    struct SAss {
        Expr dest;
        Expr val;
        SrcLoc ass_sloc;
    };
    struct SIncr {
        Expr dest;
        SrcLoc op_sloc;
    };
    struct SDecr {
        Expr dest;
        SrcLoc op_sloc;
    };
    struct SRet {
        Expr val;
    };
    struct SVRet {};
    struct SExpr {
        Expr val;
    };
    struct SWhile {
        Expr cond;
        std::unique_ptr<Stmt> body;
    };
    struct SFor {
        Type iter_type;
        Ident iter_name;
        Expr arr;
        std::unique_ptr<Stmt> body;
    };
    struct SIf {
        Expr cond;
        std::unique_ptr<Stmt> true_branch;
        std::unique_ptr<Stmt> false_branch; // This is optional
    };
    std::variant<
        SEmpty, SBlock, SDecl, SAss, SIncr, SDecr, SRet, SVRet, SExpr, SWhile, SFor, SIf>
        val;
    SrcLoc sloc;
    // It is not propagated down the tree
    Reachability reachability = Reachability::REACHABLE;
};

struct Block {
    std::vector<Stmt> stmts;
    SrcLoc sloc;
    // It is not propagated down the tree
    Reachability reachability = Reachability::REACHABLE;
};

struct FnArg {
    Type type;
    Ident name;
    SrcLoc sloc;
};

struct FieldDeclItem {
    Ident name;
    SrcLoc sloc;
};

struct ClassMemberDef {
    struct FieldDecl {
        Type type;
        std::vector<FieldDeclItem> names;
    };
    struct Method {
        Ident name;
        Type ret_type;
        std::vector<FnArg> args;
        Block body;
    };
    std::variant<FieldDecl, Method> val;
    SrcLoc sloc;
};

struct TopDef {
    struct FnDef {
        Ident name;
        Type ret_type;
        std::vector<FnArg> args;
        Block body;
    };
    struct ClassDef {
        Ident name;
        std::optional<Ident> base_class_name;
        std::vector<ClassMemberDef> members;
    };
    std::variant<FnDef, ClassDef> val;
    SrcLoc sloc;
};

struct Program {
    std::vector<TopDef> top_defs;
    SrcLoc sloc;
};

} // namespace ast
