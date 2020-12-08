#include "src/frontend/type_checker.hh"
#include "src/ast/ast.hh"
#include "src/frontend/error.hh"
#include "src/overloaded.hh"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace {

class TypeChecker {
    using GlobalSymbols = ast::GlobalSymbols;
    using Type = ast::Type;
    using Ident = ast::Ident;
    using SrcLoc = ast::SrcLoc;

    const GlobalSymbols& global_symbols;
    frontend::ErrorPrinter& errp;

    void check_type_correctness(const Type& t) const {
        std::visit(
            overloaded{
                [&](const Type::TNull& /*unused*/) {},
                [&](const Type::TInt& /*unused*/) {},
                [&](const Type::TStr& /*unused*/) {},
                [&](const Type::TBool& /*unused*/) {},
                [&](const Type::TVoid& /*unused*/) {},
                [&](const Type::TArray& ta) { check_type_correctness(*ta.elem_type); },
                [&](const Type::TClass& tc) {
                    if (global_symbols.classes.find(tc.name) == global_symbols.classes.end()) {
                        errp.error(t.sloc, "Unknown class: ", tc.name);
                    }
                },
                [&](const Type::TFun& tf) {
                    check_type_correctness(*tf.ret_type);
                    for (auto& arg_type : *tf.arg_types) {
                        check_type_correctness(arg_type);
                    }
                },
            },
            t.val);
    }

    [[nodiscard]] static bool can_be_variable_type(const Type& t) noexcept {
        return std::visit(
            overloaded{
                [](const Type::TNull& /*unused*/) { return false; },
                [](const Type::TInt& /*unused*/) { return true; },
                [](const Type::TStr& /*unused*/) { return true; },
                [](const Type::TBool& /*unused*/) { return true; },
                [](const Type::TVoid& /*unused*/) { return false; },
                [&](const Type::TArray& ta) { return can_be_variable_type(*ta.elem_type); },
                [](const Type::TClass& /*unused*/) { return true; },
                [](const Type::TFun& /*unused*/) { return false; },
            },
            t.val);
    }

    [[nodiscard]] static bool can_be_function_return_type(const Type& t) noexcept {
        return std::visit(
            overloaded{
                [](const Type::TNull& /*unused*/) { return false; },
                [](const Type::TInt& /*unused*/) { return true; },
                [](const Type::TStr& /*unused*/) { return true; },
                [](const Type::TBool& /*unused*/) { return true; },
                [](const Type::TVoid& /*unused*/) { return true; },
                [](const Type::TArray& /*unused*/) { return true; },
                [](const Type::TClass& /*unused*/) { return true; },
                [](const Type::TFun& /*unused*/) { return false; },
            },
            t.val);
    }

    [[nodiscard]] static bool is_array_type(const Type& t) noexcept {
        return std::visit(
            overloaded{
                [](const Type::TNull& /*unused*/) { return false; },
                [](const Type::TInt& /*unused*/) { return false; },
                [](const Type::TStr& /*unused*/) { return false; },
                [](const Type::TBool& /*unused*/) { return false; },
                [](const Type::TVoid& /*unused*/) { return false; },
                [](const Type::TArray& /*unused*/) { return true; },
                [](const Type::TClass& /*unused*/) { return false; },
                [](const Type::TFun& /*unused*/) { return false; },
            },
            t.val);
    }

    [[nodiscard]] static bool is_class_type(const Type& t) noexcept {
        return std::visit(
            overloaded{
                [](const Type::TNull& /*unused*/) { return false; },
                [](const Type::TInt& /*unused*/) { return false; },
                [](const Type::TStr& /*unused*/) { return false; },
                [](const Type::TBool& /*unused*/) { return false; },
                [](const Type::TVoid& /*unused*/) { return false; },
                [](const Type::TArray& /*unused*/) { return false; },
                [](const Type::TClass& /*unused*/) { return true; },
                [](const Type::TFun& /*unused*/) { return false; },
            },
            t.val);
    }

    [[nodiscard]] static bool is_reference_type(const Type& t) noexcept {
        return is_array_type(t) or is_class_type(t);
    }

    [[nodiscard]] bool is_convertible_to(const Type& src, const Type& dest) const noexcept {
        return std::visit(
            overloaded{
                [&](const Type::TNull& /*unused*/) { return src == ast::type_null; },
                [&](const Type::TInt& /*unused*/) { return src == ast::type_int; },
                [&](const Type::TStr& /*unused*/) { return src == ast::type_str; },
                [&](const Type::TBool& /*unused*/) { return src == ast::type_bool; },
                [](const Type::TVoid& /*unused*/) { return false; },
                [&](const Type::TArray& /*unused*/) {
                    return src == dest or src == ast::type_null;
                },
                [&](const Type::TClass& tc) {
                    if (src == ast::type_null) {
                        return true;
                    }
                    return std::visit(
                        [&](const auto& type_variant) {
                            if constexpr (std::is_same_v<
                                              std::decay_t<decltype(type_variant)>,
                                              Type::TClass>) {
                                return global_symbols.is_ancestor_of(
                                    tc.name, type_variant.name);
                            } else {
                                return false;
                            }
                        },
                        src.val);
                },
                [](const Type::TFun& /*unused*/) { return false; },
            },
            dest.val);
    }

    void check_func_or_method_type(
        const Type& ret_type, const std::vector<ast::FnArg>& args, SrcLoc sloc) {
        check_type_correctness(ret_type);
        if (not can_be_function_return_type(ret_type)) {
            errp.error(sloc, "function cannot have return type `", as_str(ret_type), '`');
        }
        for (auto& arg : args) {
            check_type_correctness(arg.type);
            if (not can_be_variable_type(arg.type)) {
                errp.error(
                    arg.sloc, "function argument `", arg.name, "` cannot have type `",
                    as_str(arg.type), '`');
            }
        }
    }

    void check_global_symbol_types(const ast::ClassMemberDef& member) {
        std::visit(
            overloaded{
                [&](const ast::ClassMemberDef::FieldDecl& field) {
                    check_type_correctness(field.type);
                    if (not can_be_variable_type(field.type)) {
                        errp.error(
                            member.sloc, "field cannot have type: ", as_str(field.type));
                    }
                },
                [&](const ast::ClassMemberDef::Method& method) {
                    check_func_or_method_type(method.ret_type, method.args, member.sloc);
                },
            },
            member.val);
    }

    void check_global_symbol_types(const ast::TopDef& top_def) {
        std::visit(
            overloaded{
                [&](const ast::TopDef::FnDef& fn_def) {
                    check_func_or_method_type(fn_def.ret_type, fn_def.args, top_def.sloc);
                },
                [&](const ast::TopDef::ClassDef& class_def) {
                    for (auto& member : class_def.members) {
                        check_global_symbol_types(member);
                    }
                },
            },
            top_def.val);
    }

    void check_global_symbol_types(const ast::Program& prog) {
        for (auto& top_def : prog.top_defs) {
            check_global_symbol_types(top_def);
        }
    }

    struct Context {
        struct VarInfo {
            Type type;
            SrcLoc sloc;
            size_t scope_level;
        };
        using VarEnv = PersistentMap<Ident, VarInfo>;

        const TypeChecker& type_checker;
        const size_t curr_scope_level;
        VarEnv vars;
        const GlobalSymbols::Class* self; // nullptr iff outside a class
        const Type& func_return_type;

        void add_var(Type type, Ident name, SrcLoc sloc, const char* variable_kind) {
            type_checker.check_type_correctness(type);
            if (not can_be_variable_type(type)) {
                type_checker.errp.error(
                    sloc, variable_kind, " `", name, "` cannot have type `", as_str(type),
                    '`');
            }
            auto same_var = vars.find(name);
            if (same_var and same_var->scope_level == curr_scope_level) {
                type_checker.errp
                    .error(sloc, "cannot redefine ", variable_kind, " `", name, '`')
                    .note(same_var->sloc, "previous definition is here");
            }
            vars = vars.insert_or_assign(
                std::move(name),
                VarInfo{
                    .type = std::move(type),
                    .sloc = sloc,
                    .scope_level = curr_scope_level,
                });
        }
    };

    // @p fdef_sloc can be nullopt for function that have no declaration, e.g. built-in
    // functions
    void process_func_args(
        const Ident& fname, std::vector<ast::Expr>& args, SrcLoc lparen_sloc,
        const Type::TFun& ftype, std::optional<SrcLoc> fdef_sloc, const Context& context) {
        auto error = [&](auto&&... args) {
            auto err = errp.error(std::forward<decltype(args)>(args)...);
            if (fdef_sloc) {
                err.note(*fdef_sloc, '`', fname, "` declared here");
            }
        };
        auto args_num = args.size();
        auto def_args_num = ftype.arg_types->size();
        if (args_num != def_args_num) {
            error(
                lparen_sloc, "too ", (args_num < def_args_num ? "few" : "many"),
                " arguments to function call, expected ", def_args_num, ", have ", args_num);
        }
        for (size_t i = 0; i < args_num; ++i) {
            process(args[i], context);
            auto& arg_type = args[i].type;
            auto& expected_type = (*ftype.arg_types)[i];
            if (not is_convertible_to(arg_type, expected_type)) {
                error(
                    args[i].sloc, "value of type `", as_str(arg_type),
                    "` is not convertible to type `", as_str(expected_type), '`');
            }
        }
    }

    void process(ast::Expr& expr, const Context& context) {
        std::visit(
            overloaded{
                [&](ast::Expr::EVar& var) {
                    expr.can_be_lvalue = true;
                    auto context_var = context.vars.find(var.name);
                    bool found = false;
                    if (context_var) {
                        found = true;
                        expr.type = context_var->type;
                        var.kind = ast::Expr::EVar::Kind::LOCAL_VAR;
                    } else if (context.self) {
                        auto field_var = context.self->fields.find(var.name);
                        if (field_var) {
                            found = true;
                            expr.type = field_var->type;
                            var.kind = ast::Expr::EVar::Kind::CLASS_FIELD;
                            var.field_class = context.self;
                        }
                    }
                    if (not found) {
                        errp.error(expr.sloc, "use of undeclared variable `", var.name, '`');
                    }
                },
                [&](ast::Expr::ELitInt& /*unused*/) { expr.type = ast::type_int; },
                [&](ast::Expr::ELitBool& /*unused*/) { expr.type = ast::type_bool; },
                [&](ast::Expr::ESelf& /*unused*/) {
                    if (not context.self) {
                        errp.error(expr.sloc, "self is invalid outside a class");
                    }
                    expr.type = Type{
                        .val = Type::TClass{.name = context.self->name},
                        .sloc = {.line = 0, .column = 0}};
                },
                [&](ast::Expr::ENull& /*unused*/) { expr.type = ast::type_null; },
                [&](ast::Expr::ECastedNull& cast) {
                    check_type_correctness(cast.type);
                    if (not is_reference_type(cast.type)) {
                        errp.error(
                            expr.sloc, "cannot cast null to type `", as_str(cast.type), '`');
                    }
                    expr.type = cast.type;
                },
                [&](ast::Expr::ELitStr& /*unused*/) { expr.type = ast::type_str; },
                [&](ast::Expr::EArrElem& arr_elem) {
                    process(*arr_elem.arr, context);
                    process(*arr_elem.index, context);
                    if (not is_array_type(arr_elem.arr->type)) {
                        errp.error(
                            arr_elem.lbracket_sloc, "value of non-array type `",
                            as_str(arr_elem.arr->type), "` has no subscript operator");
                    }
                    expr.type = *std::get<Type::TArray>(arr_elem.arr->type.val).elem_type;

                    if (arr_elem.index->type != ast::type_int) {
                        errp.error(
                            arr_elem.lbracket_sloc, "array subscript has to be of type `",
                            as_str(ast::type_int), "`, not `", as_str(arr_elem.index->type),
                            '`');
                    }
                    expr.can_be_lvalue = true;
                },
                [&](ast::Expr::ECallFunc& fcall) {
                    // Look for methods first
                    std::optional<SrcLoc> fdef_sloc;
                    const Type::TFun* ftype = nullptr;
                    if (context.self) {
                        auto method = context.self->methods.find(fcall.func_name);
                        if (method) {
                            ftype = &method->type;
                            fdef_sloc = method->def_sloc;
                            fcall.kind = ast::Expr::ECallFunc::Kind::METHOD;
                            fcall.method_class = context.self;
                        }
                    }
                    // Then for built-in functions
                    if (not ftype) {
                        auto it = global_symbols.builtin_functions.find(fcall.func_name);
                        if (it != global_symbols.builtin_functions.end()) {
                            ftype = &it->second;
                            fcall.kind = ast::Expr::ECallFunc::Kind::BUILTIN;
                        }
                    }
                    // Then for functions
                    if (not ftype) {
                        auto it = global_symbols.functions.find(fcall.func_name);
                        if (it == global_symbols.functions.end()) {
                            errp.error(
                                expr.sloc, "call to undeclared function `", fcall.func_name,
                                '`');
                        }
                        ftype = &it->second.type;
                        fdef_sloc = it->second.def_sloc;
                        fcall.kind = ast::Expr::ECallFunc::Kind::FUNCTION;
                    }

                    process_func_args(
                        fcall.func_name, *fcall.args, fcall.lparen_sloc, *ftype, fdef_sloc,
                        context);
                    expr.type = *ftype->ret_type;
                },
                [&](ast::Expr::EField& field) {
                    process(*field.object, context);
                    auto& obj_type = field.object->type;
                    if (is_array_type(obj_type)) {
                        if (field.field_name != "length") {
                            errp.error(
                                field.dot_sloc, "array has no field named `", field.field_name,
                                '`');
                        }
                        expr.type = ast::type_int;
                        field.kind = ast::Expr::EField::Kind::ARRAY_LENGTH;
                    } else if (is_class_type(obj_type)) {
                        auto const& cl = global_symbols.classes.at(
                            std::get<Type::TClass>(obj_type.val).name);
                        auto field_opt = cl.fields.find(field.field_name);
                        if (not field_opt) {
                            errp.error(
                                field.dot_sloc, "class `", cl.name, "` has no field named `",
                                field.field_name, '`');
                        }
                        expr.type = field_opt->type;
                        expr.can_be_lvalue = true;
                        field.kind = ast::Expr::EField::Kind::CLASS_FIELD;
                        field.field_class = &cl;
                    } else {
                        errp.error(
                            field.dot_sloc, "type `", as_str(obj_type), "` has no fields");
                    }
                },
                [&](ast::Expr::ECallMethod& mcall) {
                    process(*mcall.object, context);
                    if (not is_class_type(mcall.object->type)) {
                        errp.error(
                            mcall.dot_sloc, "non-class type `", as_str(mcall.object->type),
                            "` has no methods");
                    }
                    const GlobalSymbols::Class& cl = global_symbols.classes.at(
                        std::get<Type::TClass>(mcall.object->type.val).name);
                    SrcLoc mdef_sloc;
                    const auto& mtype = [&]() -> const Type::TFun& {
                        auto method = cl.methods.find(mcall.method_name);
                        if (not method) {
                            errp.error(
                                mcall.dot_sloc, "call to undefined method `",
                                mcall.method_name, '`');
                        }
                        mdef_sloc = method->def_sloc;
                        return method->type;
                    }();

                    process_func_args(
                        mcall.method_name, *mcall.args, mcall.lparen_sloc, mtype, mdef_sloc,
                        context);
                    expr.type = *mtype.ret_type;
                    mcall.method_class = &cl;
                },
                [&](ast::Expr::ENewArray& na) {
                    check_type_correctness(na.elem_type);
                    if (not can_be_variable_type(na.elem_type)) {
                        errp.error(
                            expr.sloc, "array element cannot have type `",
                            as_str(na.elem_type), '`');
                    }
                    process(*na.size, context);
                    if (na.size->type != ast::type_int) {
                        errp.error(
                            na.size->sloc, "array size has to be of type `",
                            as_str(ast::type_int), "`, not `", as_str(na.size->type), '`');
                    }
                    expr.type = Type{
                        .val = Type::TArray{.elem_type = std::make_unique<Type>(na.elem_type)},
                        .sloc = {.line = 0, .column = 0}};
                },
                [&](ast::Expr::ENewClass& nc) {
                    if (global_symbols.classes.count(nc.class_name) != 1) {
                        errp.error(expr.sloc, "use of undeclared class `", nc.class_name, '`');
                    }
                    expr.type = Type{
                        .val = Type::TClass{.name = nc.class_name},
                        .sloc = {.line = 0, .column = 0}};
                },
                [&](ast::Expr::EUnaryOp& uop) {
                    auto& val_expr = *uop.val;
                    process(val_expr, context);
                    switch (uop.op) {
                    case ast::UnaryOp::NEG: {
                        if (val_expr.type != ast::type_int) {
                            errp.error(
                                uop.op_sloc, "cannot negate value of non-integer type `",
                                as_str(val_expr.type), '`');
                        }
                        expr.type = ast::type_int;
                    } break;
                    case ast::UnaryOp::NOT: {
                        if (val_expr.type != ast::type_bool) {
                            errp.error(
                                uop.op_sloc, "cannot negate value of non-boolean type `",
                                as_str(val_expr.type), '`');
                        }
                        expr.type = ast::type_bool;
                    } break;
                    }
                },
                [&](ast::Expr::EBinOp& bop) {
                    auto& lexpr = *bop.left;
                    auto& rexpr = *bop.right;
                    process(lexpr, context);
                    process(rexpr, context);

                    auto throw_wrong_types_error = [&](auto&&... description_prefix) {
                        errp.error(
                            bop.op_sloc,
                            std::forward<decltype(description_prefix)>(description_prefix)...,
                            ", not `", as_str(lexpr.type), "` and `", as_str(rexpr.type), '`');
                    };

                    auto int_or_string_relop = [&](const char* op_str) {
                        if (!(lexpr.type == ast::type_int and rexpr.type == ast::type_int) and
                            !(lexpr.type == ast::type_str and rexpr.type == ast::type_str))
                        {
                            throw_wrong_types_error(
                                "operator ", op_str,
                                " is defined only for int values and string values");
                        }
                        expr.type = ast::type_bool;
                    };

                    switch (bop.op) {
                    case ast::BinOp::ADD: {
                        if (lexpr.type == ast::type_int and rexpr.type == ast::type_int) {
                            expr.type = ast::type_int;
                        } else if (lexpr.type == ast::type_str and rexpr.type == ast::type_str)
                        {
                            expr.type = ast::type_str;
                        } else {
                            throw_wrong_types_error(
                                "addition is defined only for int values or string values");
                        }
                    } break;
                    case ast::BinOp::SUB: {
                        if (lexpr.type != ast::type_int or rexpr.type != ast::type_int) {
                            throw_wrong_types_error(
                                "subtraction is defined only for int values");
                        }
                        expr.type = ast::type_int;
                    } break;
                    case ast::BinOp::MUL: {
                        if (lexpr.type != ast::type_int or rexpr.type != ast::type_int) {
                            throw_wrong_types_error(
                                "multiplication is defined only for int values");
                        }
                        expr.type = ast::type_int;
                    } break;
                    case ast::BinOp::DIV: {
                        if (lexpr.type != ast::type_int or rexpr.type != ast::type_int) {
                            throw_wrong_types_error("division is defined only for int values");
                        }
                        expr.type = ast::type_int;
                    } break;
                    case ast::BinOp::MOD: {
                        if (lexpr.type != ast::type_int or rexpr.type != ast::type_int) {
                            throw_wrong_types_error("modulo is defined only for int values");
                        }
                        expr.type = ast::type_int;
                    } break;
                    case ast::BinOp::LTH: {
                        int_or_string_relop("<");
                    } break;
                    case ast::BinOp::LE: {
                        int_or_string_relop("<=");
                    } break;
                    case ast::BinOp::GTH: {
                        int_or_string_relop(">");
                    } break;
                    case ast::BinOp::GE: {
                        int_or_string_relop(">=");
                    } break;
                    case ast::BinOp::EQ: {
                        if (not is_convertible_to(rexpr.type, lexpr.type) and
                            not is_convertible_to(lexpr.type, rexpr.type))
                        {
                            errp.error(
                                bop.op_sloc, "operator == cannot compare values of types `",
                                as_str(lexpr.type), "` and `", as_str(rexpr.type),
                                "` because values are not convertible to either of the two "
                                "types");
                        }
                        expr.type = ast::type_bool;
                    } break;
                    case ast::BinOp::NE: {
                        if (not is_convertible_to(rexpr.type, lexpr.type) and
                            not is_convertible_to(lexpr.type, rexpr.type))
                        {
                            errp.error(
                                bop.op_sloc, "operator != cannot compare values of types `",
                                as_str(lexpr.type), "` and `", as_str(rexpr.type),
                                "` because values are not convertible to either of the two "
                                "types");
                        }
                        expr.type = ast::type_bool;
                    } break;
                    case ast::BinOp::AND: {
                        if (lexpr.type != ast::type_bool or rexpr.type != ast::type_bool) {
                            throw_wrong_types_error(
                                "operator && is defined only for `", as_str(ast::type_bool),
                                "` values");
                        }
                        expr.type = ast::type_bool;
                    } break;
                    case ast::BinOp::OR: {
                        if (lexpr.type != ast::type_bool or rexpr.type != ast::type_bool) {
                            throw_wrong_types_error(
                                "operator || is defined only for `", as_str(ast::type_bool),
                                "` values");
                        }
                        expr.type = ast::type_bool;
                    } break;
                    }
                },
            },
            expr.val);
    }

    void process(ast::DeclItem& decl_item, Type type, Context& context) {
        std::visit(
            overloaded{
                [&](ast::DeclItem::DNoInit& decl) {
                    context.add_var(std::move(type), decl.name, decl_item.sloc, "variable");
                },
                [&](ast::DeclItem::DInit& decl) {
                    process(decl.val, context);
                    if (not is_convertible_to(decl.val.type, type)) {
                        errp.error(
                            decl.ass_sloc,
                            "incompatible types: cannot assign variable of type `",
                            as_str(type), "` with an expression of type `",
                            as_str(decl.val.type), '`');
                    }
                    context.add_var(std::move(type), decl.name, decl_item.sloc, "variable");
                },
            },
            decl_item.val);
    }

    void process(ast::Stmt& stmt, Context& context) {
        std::visit(
            overloaded{
                [&](ast::Stmt::SEmpty& /*unused*/) {},
                [&](ast::Stmt::SBlock& block) {
                    process(
                        *block.block,
                        Context{
                            .type_checker = *this,
                            .curr_scope_level = context.curr_scope_level + 1,
                            .vars = context.vars,
                            .self = context.self,
                            .func_return_type = context.func_return_type,
                        });
                },
                [&](ast::Stmt::SDecl& decl) {
                    for (auto& item : decl.items) {
                        process(item, decl.type, context);
                    }
                },
                [&](ast::Stmt::SAss& ass) {
                    process(ass.dest, context);
                    process(ass.val, context);
                    if (not ass.dest.can_be_lvalue) {
                        errp.error(ass.ass_sloc, "left side is not assignable")
                            .note(ass.dest.sloc, "left side starts here");
                    }
                    if (not is_convertible_to(ass.val.type, ass.dest.type)) {
                        errp.error(
                            ass.ass_sloc, "assigning to `", as_str(ass.dest.type),
                            "` from incompatible type `", as_str(ass.val.type), '`');
                    }
                },
                [&](ast::Stmt::SIncr& incr) {
                    process(incr.dest, context);
                    if (not incr.dest.can_be_lvalue) {
                        errp.error(incr.op_sloc, "expression is not assignable")
                            .note(stmt.sloc, "expression starts here");
                    }
                    if (incr.dest.type != ast::type_int) {
                        errp.error(
                            incr.op_sloc, "++ operator is valid only for integer values not `",
                            as_str(incr.dest.type), '`');
                    }
                },
                [&](ast::Stmt::SDecr& decr) {
                    process(decr.dest, context);
                    if (not decr.dest.can_be_lvalue) {
                        errp.error(decr.op_sloc, "expression is not assignable")
                            .note(stmt.sloc, "expression starts here");
                    }
                    if (decr.dest.type != ast::type_int) {
                        errp.error(
                            decr.op_sloc, "-- operator is valid only for integer values not `",
                            as_str(decr.dest.type), '`');
                    }
                },
                [&](ast::Stmt::SRet& ret) {
                    process(ret.val, context);
                    if (not is_convertible_to(ret.val.type, context.func_return_type)) {
                        errp.error(
                            ret.val.sloc, "returned value of type `", as_str(ret.val.type),
                            "` is not convertible to return type `",
                            as_str(context.func_return_type), '`',
                            (ret.val.type == ast::type_void and
                                     context.func_return_type == ast::type_void
                                 ? ", use `return;` instruction instead"
                                 : ""));
                    }
                },
                [&](ast::Stmt::SVRet& /*unused*/) {
                    if (context.func_return_type != ast::type_void) {
                        errp.error(stmt.sloc, "non-void function should return a value")
                            .note(
                                context.func_return_type.sloc,
                                "function return type is declared here");
                    }
                },
                [&](ast::Stmt::SExpr& expr) { process(expr.val, context); },
                [&](ast::Stmt::SWhile& swhile) {
                    process(swhile.cond, context);
                    if (swhile.cond.type != ast::type_bool) {
                        errp.error(
                            swhile.cond.sloc, "while condition has to be of type `",
                            as_str(ast::type_bool), "`, not `", as_str(swhile.cond.type), '`');
                    }
                    auto body_context = Context{
                        .type_checker = *this,
                        .curr_scope_level = context.curr_scope_level + 1,
                        .vars = context.vars,
                        .self = context.self,
                        .func_return_type = context.func_return_type,
                    };
                    process(*swhile.body, body_context);
                },
                [&](ast::Stmt::SFor& sfor) {
                    auto body_context = Context{
                        .type_checker = *this,
                        .curr_scope_level = context.curr_scope_level + 1,
                        .vars = context.vars,
                        .self = context.self,
                        .func_return_type = context.func_return_type,
                    };
                    body_context.add_var(
                        sfor.iter_type, sfor.iter_name, sfor.iter_type.sloc, "for iterator");

                    process(sfor.arr, context);
                    if (not is_array_type(sfor.arr.type)) {
                        errp.error(
                            sfor.arr.sloc, "for cannot iterate over value of non-array type `",
                            as_str(sfor.arr.type), '`');
                    }
                    const auto& arr_elem_type =
                        *std::get<Type::TArray>(sfor.arr.type.val).elem_type;
                    if (not is_convertible_to(arr_elem_type, sfor.iter_type)) {
                        errp.error(
                                sfor.arr.sloc, "array element type `", as_str(arr_elem_type),
                                "` is not convertible to `", as_str(sfor.iter_type), '`')
                            .note(sfor.iter_type.sloc, "required by here");
                    }
                    if (std::holds_alternative<ast::Stmt::SBlock>(sfor.body->val)) {
                        process(
                            *std::get<ast::Stmt::SBlock>(sfor.body->val).block,
                            std::move(body_context));
                    } else {
                        process(*sfor.body, body_context);
                    }
                },
                [&](ast::Stmt::SIf& sif) {
                    process(sif.cond, context);
                    if (sif.cond.type != ast::type_bool) {
                        errp.error(
                            sif.cond.sloc, "if condition has to be of type `",
                            as_str(ast::type_bool), "`, not `", as_str(sif.cond.type), '`');
                    }
                    for (auto* branch : {sif.true_branch.get(), sif.false_branch.get()}) {
                        if (not branch) {
                            continue;
                        }
                        auto branch_context = Context{
                            .type_checker = *this,
                            .curr_scope_level = context.curr_scope_level + 1,
                            .vars = context.vars,
                            .self = context.self,
                            .func_return_type = context.func_return_type,
                        };
                        process(*branch, branch_context);
                    }
                },
            },
            stmt.val);
    }

    void process(ast::Block& block, Context&& context) {
        for (auto& stmt : block.stmts) {
            process(stmt, context);
        }
    }

    void process_func_or_method(
        const std::vector<ast::FnArg>& args, ast::Block& body, Context&& context) {
        // Checking of the function type was done while checking global definitions
        for (auto& arg : args) {
            context.add_var(arg.type, arg.name, arg.sloc, "function argument");
        }
        process(
            body,
            Context{
                .type_checker = *this,
                .curr_scope_level = context.curr_scope_level + 1,
                .vars = context.vars,
                .self = context.self,
                .func_return_type = context.func_return_type,
            });
    }

    void process(ast::TopDef::FnDef& fn) {
        process_func_or_method(
            fn.args, fn.body,
            Context{
                .type_checker = *this,
                .curr_scope_level = 0,
                .vars = {},
                .self = nullptr,
                .func_return_type = fn.ret_type,
            });
    }

    void process(ast::TopDef::ClassDef& cl) {
        const auto& class_symb = global_symbols.classes.at(cl.name);
        for (auto& member : cl.members) {
            std::visit(
                overloaded{
                    [&](ast::ClassMemberDef::FieldDecl& /*unused*/) {
                        // Checking of the field type was done while checking global
                        // definitions
                    },
                    [&](ast::ClassMemberDef::Method& method) {
                        process_func_or_method(
                            method.args, method.body,
                            Context{
                                .type_checker = *this,
                                .curr_scope_level = 0,
                                .vars = {},
                                .self = &class_symb,
                                .func_return_type = method.ret_type,
                            });
                    },
                },
                member.val);
        }
    }

    void check_main_function() {
        auto it = global_symbols.functions.find("main");
        if (it == global_symbols.functions.end()) {
            errp.error({.line = 0, .column = 0}, "function `main` was not defined");
        }
        auto& main_func = it->second;
        if (*main_func.type.ret_type != ast::type_int) {
            errp.error(
                main_func.def_sloc, "function `main` has to return `", as_str(ast::type_int),
                '`');
        }
        if (not main_func.type.arg_types->empty()) {
            errp.error(
                main_func.type.arg_types->at(0).sloc, "function `main` cannot take arguments");
        }
    }

public:
    explicit TypeChecker(const GlobalSymbols& global_symbols, frontend::ErrorPrinter& errp)
    : global_symbols{global_symbols}
    , errp{errp} {}

    void process(ast::Program& prog) && {
        check_main_function();
        check_global_symbol_types(prog);
        for (auto& top_def : prog.top_defs) {
            std::visit([&](auto& top_def_variant) { process(top_def_variant); }, top_def.val);
        }
    }
};

} // namespace

namespace frontend {

void check_and_annotate_types(
    ast::Program& prog, const ast::GlobalSymbols& global_symbols, ErrorPrinter& errp) {
    TypeChecker{global_symbols, errp}.process(prog);
}

} // namespace frontend
