#include "src/frontend/static_analyzer.hh"
#include "src/ast/ast.hh"
#include "src/frontend/error.hh"
#include "src/overloaded.hh"

#include <cassert>
#include <variant>

namespace {

class StaticAnalyzer {
    using SrcLoc = ast::SrcLoc;
    frontend::ErrorPrinter& errp;

    // @p str consists only of digits and @p negative holds the sign
    int_t parse_int_literal(const std::string& str, bool negative, SrcLoc sloc) {
        assert(not str.empty());
        assert(std::all_of(str.begin(), str.end(), ::isdigit));
        int_t val = negative ? '0' - str[0] : str[0] - '0';
        for (size_t i = 1; i < str.size(); ++i) {
            if (__builtin_mul_overflow(val, 10, &val) or
                __builtin_add_overflow(val, (negative ? '0' - str[i] : str[i] - '0'), &val))
            {
                errp.error(
                    sloc, "integer overflow while parsing the string literal ",
                    (negative ? "-" : ""), str);
            }
        }
        return val;
    }

    struct [[nodiscard]] ControlFlow {
        bool can_go_through =
            true; // whether control flow can reach the code after the current code

        friend ControlFlow& operator&=(ControlFlow& a, ControlFlow b) noexcept {
            a.can_go_through &= b.can_go_through;
            return a;
        }
        friend ControlFlow& operator|=(ControlFlow& a, ControlFlow b) noexcept {
            a.can_go_through |= b.can_go_through;
            return a;
        }
    };

    ControlFlow analyze(ast::Expr& expr, const ControlFlow& parent_cf) {
        if (not parent_cf.can_go_through) {
            errp.warning(expr.sloc, "this code will never be executed");
            expr.reachability = ast::Reachability::UNREACHABLE;
            return parent_cf;
        }
        return std::visit(
            overloaded{
                [&](ast::Expr::EVar& /*unused*/) {
                    return ControlFlow{.can_go_through = true};
                },
                [&](ast::Expr::ELitInt& lit) {
                    expr.comptime_val = parse_int_literal(lit.str_val, false, expr.sloc);
                    return ControlFlow{.can_go_through = true};
                },
                [&](ast::Expr::ELitBool& lit) {
                    expr.comptime_val = lit.val;
                    return ControlFlow{.can_go_through = true};
                },
                [&](ast::Expr::ESelf& /*unused*/) {
                    return ControlFlow{.can_go_through = true};
                },
                [&](ast::Expr::ENull& /*unused*/) {
                    expr.comptime_val = ast::Null{};
                    return ControlFlow{.can_go_through = true};
                },
                [&](ast::Expr::ECastedNull& /*unused*/) {
                    expr.comptime_val = ast::Null{};
                    return ControlFlow{.can_go_through = true};
                },
                [&](ast::Expr::ELitStr& lit) {
                    expr.comptime_val = lit.val;
                    return ControlFlow{.can_go_through = true};
                },
                [&](ast::Expr::EArrElem& arr_elem) {
                    auto cf = analyze(*arr_elem.arr, {});
                    if (arr_elem.arr->comptime_val) {
                        (void)std::get<ast::Null>(*arr_elem.arr->comptime_val); // assert
                        errp.warning(arr_elem.lbracket_sloc, "subscripting a null array");
                    }
                    cf &= analyze(*arr_elem.index, cf);
                    if (arr_elem.index->comptime_val) {
                        auto idx = std::get<int_t>(*arr_elem.index->comptime_val);
                        if (idx < 0) {
                            errp.warning(
                                arr_elem.lbracket_sloc, "array index ", idx,
                                " is before the beginning of the array");
                        }
                    }
                    if (not cf.can_go_through) {
                        errp.warning(
                            arr_elem.lbracket_sloc, "array access will never be executed");
                        expr.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Expr::ECallFunc& fcall) {
                    ControlFlow cf;
                    for (auto& x : *fcall.args) {
                        cf &= analyze(x, cf);
                    }
                    if (not cf.can_go_through) {
                        errp.warning(
                            fcall.lparen_sloc,
                            "this function call will not happen, because execution stops at "
                            "computing one of the arguments");
                        expr.reachability = ast::Reachability::PARTIAL;
                    }
                    if (fcall.kind == ast::Expr::ECallFunc::Kind::BUILTIN and
                        fcall.func_name == "error") {
                        cf.can_go_through = false;
                    }
                    return cf;
                },
                [&](ast::Expr::EField& field) {
                    auto cf = analyze(*field.object, {});
                    if (not cf.can_go_through) {
                        errp.warning(
                            field.dot_sloc,
                            "this field access will never be executed, because execution "
                            "stops at computing the object");
                        expr.reachability = ast::Reachability::PARTIAL;
                    }
                    if (field.object->comptime_val) {
                        (void)std::get<ast::Null>(*field.object->comptime_val); // assert
                        errp.warning(field.dot_sloc, "accessing field of a null object")
                            .note(
                                field.object->sloc,
                                "expression evaluates to null reference of type `",
                                as_str(field.object->type), '`');
                    }
                    return cf;
                },
                [&](ast::Expr::ECallMethod& mcall) {
                    auto cf = analyze(*mcall.object, {});
                    if (mcall.object->comptime_val) {
                        (void)std::get<ast::Null>(*mcall.object->comptime_val); // assert
                        errp.warning(mcall.dot_sloc, "calling method on a null object")
                            .note(
                                mcall.object->sloc,
                                "expression evaluates to null reference of type `",
                                as_str(mcall.object->type), '`');
                    }
                    for (auto& x : *mcall.args) {
                        cf &= analyze(x, cf);
                    }
                    if (not cf.can_go_through) {
                        errp.warning(
                            mcall.lparen_sloc,
                            "this method call will not happen, because execution stops at "
                            "computing object or one of the arguments");
                        expr.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Expr::ENewArray& na) {
                    auto cf = analyze(*na.size, {});
                    if (na.size->comptime_val) {
                        auto size = std::get<int_t>(*na.size->comptime_val);
                        if (size < 0) {
                            errp.warning(na.size->sloc, "negative array size: ", size);
                        }
                    }
                    if (not cf.can_go_through) {
                        errp.warning(
                            expr.sloc,
                            "creating of the array will not happen, because execution stops "
                            "at computing its size");
                        expr.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Expr::ENewClass& /*unused*/) { return ControlFlow{}; },
                [&](ast::Expr::EUnaryOp& uop) {
                    auto& val_expr = *uop.val;
                    ControlFlow cf;
                    switch (uop.op) {
                    case ast::UnaryOp::NEG: {
                        if (std::holds_alternative<ast::Expr::ELitInt>(val_expr.val)) {
                            expr.comptime_val = parse_int_literal(
                                std::get<ast::Expr::ELitInt>(val_expr.val).str_val, true,
                                uop.op_sloc);
                        } else {
                            cf = analyze(val_expr, cf);
                            if (val_expr.comptime_val) {
                                auto expr_comptime_val =
                                    std::get<int_t>(*val_expr.comptime_val);
                                int_t val{};
                                if (__builtin_sub_overflow(0, expr_comptime_val, &val)) {
                                    errp.error(
                                        uop.op_sloc, "integer overflow while negating value: ",
                                        expr_comptime_val);
                                }
                                expr.comptime_val = val;
                            }
                        }
                    } break;
                    case ast::UnaryOp::NOT: {
                        cf = analyze(val_expr, cf);
                        if (val_expr.comptime_val) {
                            expr.comptime_val = not std::get<bool>(*val_expr.comptime_val);
                        }
                    } break;
                    }
                    if (not cf.can_go_through) {
                        errp.warning(
                            uop.op_sloc,
                            "operation will never happen, because execution stops at "
                            "computing its argument");
                        expr.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Expr::EBinOp& bop) {
                    ControlFlow cf;
                    auto& lexpr = *bop.left;
                    auto& rexpr = *bop.right;

                    auto do_comptime_int_op = [&](char op_symbol, auto&& do_op,
                                                  const char* error_description =
                                                      "integer overflow") {
                        if (not lexpr.comptime_val or not rexpr.comptime_val) {
                            return;
                        }
                        int_t res{};
                        auto a = std::get<int_t>(*lexpr.comptime_val);
                        auto b = std::get<int_t>(*rexpr.comptime_val);
                        if (do_op(a, b, &res)) {
                            errp.error(
                                bop.op_sloc, error_description, " in expression ", a, ' ',
                                op_symbol, ' ', b);
                        }
                        expr.comptime_val = res;
                    };

                    auto int_or_string_relop = [&](auto&& op) {
                        if (lexpr.type == ast::type_int) {
                            assert(rexpr.type == ast::type_int);
                            if (lexpr.comptime_val and rexpr.comptime_val) {
                                expr.comptime_val =
                                    op(std::get<int_t>(*lexpr.comptime_val),
                                       std::get<int_t>(*rexpr.comptime_val));
                            }
                        } else if (lexpr.comptime_val and rexpr.comptime_val) {
                            assert(lexpr.type == ast::type_str);
                            assert(rexpr.type == ast::type_str);
                            expr.comptime_val =
                                op(std::get<std::string>(*lexpr.comptime_val),
                                   std::get<std::string>(*rexpr.comptime_val));
                        }
                    };

                    switch (bop.op) {
                    case ast::BinOp::ADD: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        if (lexpr.type == ast::type_int) {
                            assert(rexpr.type == ast::type_int);
                            do_comptime_int_op('+', [](int_t a, int_t b, int_t* res) {
                                return __builtin_add_overflow(a, b, res);
                            });
                        } else if (lexpr.comptime_val and rexpr.comptime_val) {
                            assert(lexpr.type == ast::type_str);
                            assert(rexpr.type == ast::type_str);
                            expr.comptime_val = std::get<std::string>(*lexpr.comptime_val) +
                                std::get<std::string>(*rexpr.comptime_val);
                        }
                    } break;
                    case ast::BinOp::SUB: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        do_comptime_int_op('-', [](int_t a, int_t b, int_t* res) {
                            return __builtin_sub_overflow(a, b, res);
                        });
                    } break;
                    case ast::BinOp::MUL: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        do_comptime_int_op('*', [](int_t a, int_t b, int_t* res) {
                            return __builtin_mul_overflow(a, b, res);
                        });
                    } break;
                    case ast::BinOp::DIV: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        do_comptime_int_op('/', [&](int_t a, int_t b, int_t* res) {
                            if (b == 0) {
                                errp.error(
                                    bop.op_sloc, "division by 0 in expression ", a, " / ", b);
                                cf.can_go_through = false;
                            }
                            if (a == std::numeric_limits<decltype(a)>::min() and b == -1) {
                                return true;
                            }
                            *res = a / b;
                            return false;
                        });
                    } break;
                    case ast::BinOp::MOD: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        do_comptime_int_op('%', [&](int_t a, int_t b, int_t* res) {
                            if (b == 0) {
                                errp.error(
                                    bop.op_sloc, "division by 0 in expression ", a, " % ", b);
                            }
                            if (a == std::numeric_limits<decltype(a)>::min() and b == -1) {
                                return true;
                            }
                            *res = a % b;
                            return false;
                        });
                    } break;
                    case ast::BinOp::LTH: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        int_or_string_relop([](auto& a, auto& b) { return a < b; });
                    } break;
                    case ast::BinOp::LE: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        int_or_string_relop([](auto& a, auto& b) { return a <= b; });
                    } break;
                    case ast::BinOp::GTH: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        int_or_string_relop([](auto& a, auto& b) { return a > b; });
                    } break;
                    case ast::BinOp::GE: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        int_or_string_relop([](auto& a, auto& b) { return a >= b; });
                    } break;
                    case ast::BinOp::EQ: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        if (lexpr.comptime_val and rexpr.comptime_val) {
                            expr.comptime_val = *lexpr.comptime_val == *rexpr.comptime_val;
                        }
                        if (std::holds_alternative<ast::Expr::EVar>(lexpr.val) and
                            std::holds_alternative<ast::Expr::EVar>(rexpr.val) and
                            std::get<ast::Expr::EVar>(lexpr.val).name ==
                                std::get<ast::Expr::EVar>(rexpr.val).name)
                        {
                            expr.comptime_val = true;
                            errp.warning(bop.op_sloc, "self comparison is always true");
                        }
                    } break;
                    case ast::BinOp::NE: {
                        cf &= analyze(lexpr, cf);
                        cf &= analyze(rexpr, cf);
                        if (lexpr.comptime_val and rexpr.comptime_val) {
                            expr.comptime_val = *lexpr.comptime_val != *rexpr.comptime_val;
                        }
                        if (std::holds_alternative<ast::Expr::EVar>(lexpr.val) and
                            std::holds_alternative<ast::Expr::EVar>(rexpr.val) and
                            std::get<ast::Expr::EVar>(lexpr.val).name ==
                                std::get<ast::Expr::EVar>(rexpr.val).name)
                        {
                            expr.comptime_val = false;
                            errp.warning(bop.op_sloc, "self comparison is always false");
                        }
                    } break;
                    case ast::BinOp::AND: {
                        cf &= analyze(lexpr, cf);
                        if (lexpr.comptime_val and !std::get<bool>(*lexpr.comptime_val)) {
                            expr.comptime_val = false;
                            errp.warning(rexpr.sloc, "expression will never be evaluated")
                                .note(
                                    bop.op_sloc,
                                    "because left side of `&&` evaluates to false")
                                .note(lexpr.sloc, "left side starts here");
                            rexpr.reachability = ast::Reachability::UNREACHABLE;
                        } else {
                            cf &= analyze(rexpr, cf);
                            if (rexpr.comptime_val) {
                                bool rexpr_val = std::get<bool>(*rexpr.comptime_val);
                                if (!rexpr_val) {
                                    if (lexpr.comptime_val) {
                                        assert(std::get<bool>(*lexpr.comptime_val));
                                        expr.comptime_val = false;
                                    } else {
                                        // Cannot set to false, as lexpr has to be fully
                                        // evaluated
                                        expr.comptime_val = std::nullopt;
                                    }
                                } else if (lexpr.comptime_val) {
                                    assert(std::get<bool>(*lexpr.comptime_val));
                                    expr.comptime_val = true;
                                }
                                errp.warning(
                                        bop.op_sloc,
                                        "`&&` can be eliminated because right side always "
                                        "evaluates to ",
                                        rexpr_val ? "true" : "false")
                                    .note(rexpr.sloc, "right side starts here");
                            }
                        }
                    } break;
                    case ast::BinOp::OR: {
                        cf &= analyze(lexpr, cf);
                        if (lexpr.comptime_val and std::get<bool>(*lexpr.comptime_val)) {
                            expr.comptime_val = true;
                            errp.warning(rexpr.sloc, "expression will never be evaluated")
                                .note(
                                    bop.op_sloc, "because left side of `||` evaluates to true")
                                .note(lexpr.sloc, "left side starts here");
                            rexpr.reachability = ast::Reachability::UNREACHABLE;
                        } else {
                            cf &= analyze(rexpr, cf);
                            if (rexpr.comptime_val) {
                                bool rexpr_val = std::get<bool>(*rexpr.comptime_val);
                                if (rexpr_val) {
                                    if (lexpr.comptime_val) {
                                        assert(!std::get<bool>(*lexpr.comptime_val));
                                        expr.comptime_val = true;
                                    } else {
                                        // Cannot set to true, as lexpr has to be fully
                                        // evaluated
                                        expr.comptime_val = std::nullopt;
                                    }
                                } else if (lexpr.comptime_val) {
                                    assert(!std::get<bool>(*lexpr.comptime_val));
                                    expr.comptime_val = false;
                                }
                                errp.warning(
                                        bop.op_sloc,
                                        "`||` can be eliminated because right side always "
                                        "evaluates to ",
                                        rexpr_val ? "true" : "false")
                                    .note(rexpr.sloc, "right side starts here");
                            }
                        }
                    } break;
                    }
                    if (not cf.can_go_through) {
                        errp.warning(
                            bop.op_sloc,
                            "operation will never happen, because execution stops at "
                            "computing one of its arguments");
                        expr.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
            },
            expr.val);
    }

    ControlFlow analyze(ast::DeclItem& decl_item, const ControlFlow& parent_cf) {
        return std::visit(
            overloaded{
                [&](ast::DeclItem::DNoInit& /*unused*/) {
                    if (not parent_cf.can_go_through) {
                        // No warning about variables that won't be created
                        decl_item.reachability = ast::Reachability::UNREACHABLE;
                        return parent_cf;
                    }
                    return ControlFlow{};
                },
                [&](ast::DeclItem::DInit& decl) {
                    if (not parent_cf.can_go_through) {
                        // Warning will be produced in the subsequent analyze()
                        decl_item.reachability = ast::Reachability::UNREACHABLE;
                    }
                    return analyze(decl.val, parent_cf);
                },
            },
            decl_item.val);
    }

    ControlFlow analyze(ast::Stmt& stmt, const ControlFlow& parent_cf) {
        if (not parent_cf.can_go_through) {
            errp.warning(stmt.sloc, "this code will never be executed");
            stmt.reachability = ast::Reachability::UNREACHABLE;
            return parent_cf;
        }
        return std::visit(
            overloaded{
                [&](ast::Stmt::SEmpty& /*unused*/) { return ControlFlow{}; },
                [&](ast::Stmt::SBlock& sb) {
                    auto cf = analyze(*sb.block, {});
                    if (not cf.can_go_through) {
                        // Warning was produced when analyzing the Block
                        stmt.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Stmt::SDecl& decl) {
                    ControlFlow cf;
                    for (auto& item : decl.items) {
                        cf &= analyze(item, cf);
                    }
                    if (not cf.can_go_through) {
                        // No warning here since this is an aggregate instruction
                        stmt.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Stmt::SAss& ass) {
                    auto cf = analyze(ass.dest, {});
                    cf &= analyze(ass.val, cf);
                    if (not cf.can_go_through) {
                        errp.warning(
                            ass.ass_sloc,
                            "assignment will never happen because execution stops at "
                            "computing one of the operands");
                        stmt.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Stmt::SIncr& incr) {
                    auto cf = analyze(incr.dest, {});
                    if (not cf.can_go_through) {
                        errp.warning(
                            incr.op_sloc,
                            "assignment will never happen because execution stops at "
                            "computing the operand");
                        stmt.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Stmt::SDecr& decr) {
                    auto cf = analyze(decr.dest, {});
                    if (not cf.can_go_through) {
                        errp.warning(
                            decr.op_sloc,
                            "assignment will never happen because execution stops at "
                            "computing the operand");
                        stmt.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Stmt::SRet& ret) {
                    auto cf = analyze(ret.val, {});
                    if (not cf.can_go_through) {
                        errp.warning(
                            stmt.sloc,
                            "return will never happen because execution stops at computing "
                            "the value");
                        stmt.reachability = ast::Reachability::PARTIAL;
                    }
                    cf.can_go_through = false;
                    return cf;
                },
                [&](ast::Stmt::SVRet& /*unused*/) {
                    return ControlFlow{.can_go_through = false};
                },
                [&](ast::Stmt::SExpr& expr) {
                    auto cf = analyze(expr.val, {});
                    if (not cf.can_go_through) {
                        // Warning was produced when analyzing the Expr
                        stmt.reachability = ast::Reachability::PARTIAL;
                    }
                    return cf;
                },
                [&](ast::Stmt::SWhile& swhile) {
                    auto cf = analyze(swhile.cond, {});
                    if (swhile.cond.comptime_val) {
                        bool cond_val = std::get<bool>(*swhile.cond.comptime_val);
                        if (not cond_val) {
                            errp.warning(
                                stmt.sloc,
                                "whole while is dead code because while condition always "
                                "evaluates to false");
                            stmt.reachability = ast::Reachability::UNREACHABLE;
                            return cf;
                        }
                        assert(cond_val);
                        cf &= analyze(*swhile.body, cf);
                        cf.can_go_through = false; // while(true) never ends
                        return cf;
                    }
                    if (not cf.can_go_through) {
                        errp.warning(
                                swhile.cond.sloc,
                                "while body will never be executed because execution always "
                                "stops at computing the while condition")
                            .note(swhile.body->sloc, "dead code");
                        swhile.body->reachability = ast::Reachability::UNREACHABLE;
                        stmt.reachability = ast::Reachability::PARTIAL;
                        return cf;
                    }
                    cf = analyze(*swhile.body, cf);
                    if (not cf.can_go_through) {
                        errp.warning(
                            stmt.sloc,
                            "while will execute at most once, because its body always stops "
                            "execution");
                    }
                    return ControlFlow{}; // while may not execute if condition is initially
                                          // false
                },
                [&](ast::Stmt::SFor& sfor) {
                    auto cf = analyze(sfor.arr, {});
                    if (not cf.can_go_through) {
                        errp.warning(
                                stmt.sloc,
                                "for's body will never be executed because execution always "
                                "stops at computing the array to iterate over")
                            .note(sfor.body->sloc, "dead code");
                        sfor.body->reachability = ast::Reachability::UNREACHABLE;
                        stmt.reachability = ast::Reachability::PARTIAL;
                        return cf;
                    }
                    cf = analyze(*sfor.body, cf);
                    if (not cf.can_go_through) {
                        errp.warning(
                            stmt.sloc,
                            "for will execute at most once, because its body always stops "
                            "execution");
                    }
                    return ControlFlow{}; // for may not execute if array is empty
                },
                [&](ast::Stmt::SIf& sif) {
                    auto cf = analyze(sif.cond, {});
                    if (sif.cond.comptime_val) {
                        bool cond_val = std::get<bool>(*sif.cond.comptime_val);
                        if (cond_val) {
                            if (sif.false_branch) {
                                errp.warning(sif.false_branch->sloc, "dead code")
                                    .note(
                                        sif.cond.sloc,
                                        "if condition always evaluates to true");
                                sif.false_branch->reachability =
                                    ast::Reachability::UNREACHABLE;
                            }
                            cf &= analyze(*sif.true_branch, cf);
                            if (not cf.can_go_through) {
                                // Warning was produced when analyzing the Block
                                stmt.reachability = ast::Reachability::PARTIAL;
                            }
                            return cf;
                        }
                        assert(not cond_val);
                        errp.warning(sif.true_branch->sloc, "dead code")
                            .note(sif.cond.sloc, "if condition always evaluates to false");
                        sif.true_branch->reachability = ast::Reachability::UNREACHABLE;
                        if (sif.false_branch) {
                            cf &= analyze(*sif.false_branch, cf);
                            if (not cf.can_go_through) {
                                // Warning was produced when analyzing the Block
                                stmt.reachability = ast::Reachability::PARTIAL;
                            }
                            return cf;
                        }
                        // No else branch
                        stmt.reachability = ast::Reachability::UNREACHABLE;
                        return cf;
                    }
                    if (not cf.can_go_through) {
                        for (auto* branch : {sif.true_branch.get(), sif.false_branch.get()}) {
                            if (branch) {
                                errp.warning(branch->sloc, "dead code")
                                    .note(
                                        sif.cond.sloc,
                                        "execution always stops at computing the if "
                                        "condition");
                                branch->reachability = ast::Reachability::UNREACHABLE;
                            }
                        }
                        stmt.reachability = ast::Reachability::PARTIAL;
                    } else {
                        cf = analyze(*sif.true_branch, {});
                        if (sif.false_branch) {
                            cf |= analyze(*sif.false_branch, {});
                        } else {
                            cf.can_go_through = true; // condition may be false
                        }
                    }
                    return cf;
                },
            },
            stmt.val);
    }

    ControlFlow analyze(ast::Block& block, const ControlFlow& parent_cf) {
        if (not parent_cf.can_go_through) {
            errp.warning(block.sloc, "this code will never be executed");
            block.reachability = ast::Reachability::UNREACHABLE;
            return parent_cf;
        }
        ControlFlow cf;
        for (auto& stmt : block.stmts) {
            cf &= analyze(stmt, cf);
        }
        if (not cf.can_go_through) {
            // Warning was produced when analyzing the Stmt
            block.reachability = ast::Reachability::PARTIAL;
        }
        return cf;
    }

    void analyze_func(const ast::Type& ret_type, ast::Block& body, SrcLoc sloc) {
        auto cl = analyze(body, {});
        if (cl.can_go_through and ret_type != ast::type_void) {
            errp.error(sloc, "function does not return a value in all control paths");
        }
    }

    void analyze(ast::TopDef::ClassDef& cl) {
        for (auto& member : cl.members) {
            std::visit(
                overloaded{
                    [&](ast::ClassMemberDef::FieldDecl& /*unused*/) {
                        // Nothing to analyze here
                    },
                    [&](ast::ClassMemberDef::Method& method) {
                        // Nothing to analyze with arguments
                        analyze_func(method.ret_type, method.body, member.sloc);
                    },
                },
                member.val);
        }
    }

public:
    explicit StaticAnalyzer(frontend::ErrorPrinter& errp)
    : errp{errp} {}

    void analyze(ast::Program& prog) && {
        for (auto& top_def : prog.top_defs) {
            std::visit(
                overloaded{
                    [&](ast::TopDef::FnDef& fn) {
                        // Nothing to analyze with arguments
                        analyze_func(fn.ret_type, fn.body, top_def.sloc);
                    },
                    [&](ast::TopDef::ClassDef& cl) { analyze(cl); },
                },
                top_def.val);
        }
    }
};

} // namespace

namespace frontend {

// On error throws frontend::ErrorPrinter::ErrorOccurred
void static_analyze(ast::Program& prog, ErrorPrinter& errp) {
    StaticAnalyzer{errp}.analyze(prog);
}

} // namespace frontend
