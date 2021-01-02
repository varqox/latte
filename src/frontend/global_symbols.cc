#include "src/ast/ast.hh"
#include "src/concat.hh"
#include "src/frontend/error.hh"
#include "src/overloaded.hh"

#include <optional>

namespace {

class GlobalSymbolsCollector {
    using GlobalSymbols = ast::GlobalSymbols;
    using Type = ast::Type;

    frontend::ErrorPrinter& errp;
    GlobalSymbols globsym;

    static Type::TFun func_type(const Type& ret_type, const std::vector<ast::FnArg>& args) {
        std::vector<Type> arg_types;
        arg_types.reserve(args.size());
        for (auto& arg : args) {
            arg_types.emplace_back(arg.type);
        }
        return Type::TFun{
            .ret_type = std::make_unique<Type>(ret_type),
            .arg_types = std::make_unique<std::vector<Type>>(arg_types),
        };
    }

    void shallow_collect(
        const ast::ClassMemberDef& member_def, const ast::Ident& class_name,
        GlobalSymbols::Class& class_symb) {
        std::visit(
            overloaded{
                [&](const ast::ClassMemberDef::FieldDecl& field_decl) {
                    for (auto& fdecl_item : field_decl.names) {
                        auto same_field = class_symb.fields.find(fdecl_item.name);
                        if (same_field) {
                            errp.error(
                                    fdecl_item.sloc,
                                    "class cannot contain two fields with the same name")
                                .note(same_field->def_sloc, "previous declaration is here");
                        }
                        class_symb.fields = class_symb.fields
                                                .insert(
                                                    fdecl_item.name,
                                                    {
                                                        .defined_in_class = class_name,
                                                        .abs_idx = class_symb.fields.size(),
                                                        .type = field_decl.type,
                                                        .def_sloc = fdecl_item.sloc,
                                                    })
                                                .value();
                    }
                },
                [&](const ast::ClassMemberDef::Method& method) {
                    auto same_method = class_symb.methods.find(method.name);
                    if (same_method) {
                        errp.error(
                                member_def.sloc,
                                "class cannot contain two methods with the same name")
                            .note(same_method->def_sloc, "previous definition is here");
                    }
                    class_symb.methods =
                        class_symb.methods
                            .insert(
                                method.name,
                                {
                                    .defined_in_class = class_name,
                                    .vtable_idx = class_symb.methods.size(),
                                    .type = func_type(method.ret_type, method.args),
                                    .def_sloc = member_def.sloc,
                                })
                            .value();
                },
            },
            member_def.val);
    }

    void shallow_collect(const ast::TopDef& top_def) {
        std::visit(
            overloaded{
                [&](const ast::TopDef::FnDef& fndef) {
                    if (GlobalSymbols::builtin_functions.count(fndef.name) != 0) {
                        errp.error(
                            top_def.sloc,
                            "cannot define function having the same name as the predefined "
                            "function `",
                            fndef.name, '`');
                    }
                    auto [it, inserted] = globsym.functions.try_emplace(
                        fndef.name,
                        GlobalSymbols::Function{
                            .type = func_type(fndef.ret_type, fndef.args),
                            .def_sloc = top_def.sloc,
                        });
                    if (not inserted) {
                        errp.error(
                                top_def.sloc, "cannot redefine function `", fndef.name,
                                "` that is already defined")
                            .note(it->second.def_sloc, "previous definition is here");
                    }
                },
                [&](const ast::TopDef::ClassDef& class_def) {
                    auto [it, inserted] = globsym.classes.try_emplace(
                        class_def.name,
                        GlobalSymbols::Class{
                            .name = class_def.name,
                            .base_class = class_def.base_class_name,
                            .fields = {},
                            .methods = {},
                            .type =
                                ast::Type{
                                    .val = ast::Type::TClass{.name = class_def.name},
                                    .sloc = top_def.sloc},
                            .def_sloc = top_def.sloc,
                            .pre_post_order = {.in = 0, .out = 0}, // Will be filled later
                        });
                    if (not inserted) {
                        errp.error(
                                top_def.sloc, "cannot redefine class `", class_def.name,
                                "` that is already defined")
                            .note(it->second.def_sloc, "previous definition is here");
                    }
                    for (auto& member_def : class_def.members) {
                        shallow_collect(member_def, it->first, it->second);
                    }
                },
            },
            top_def.val);
    }

    void propagate_members(GlobalSymbols::Class& cl, GlobalSymbols::Class& base_cl) {
        // Propagate fields
        {
            auto defined_fields = std::move(cl.fields);
            auto all_fields = base_cl.fields;
            auto base_class_field_num = all_fields.size();
            defined_fields.for_each(
                [&](const ast::Ident& name, const GlobalSymbols::Class::Field& field) {
                    auto same_field = all_fields.find(name);
                    if (same_field) {
                        errp.error(
                                field.def_sloc, "cannot redefine field `", name,
                                "` defined in class `", same_field->defined_in_class, '`')
                            .note(same_field->def_sloc, "previous definition is here");
                    }
                    all_fields = all_fields
                                     .insert(
                                         name,
                                         {
                                             .defined_in_class = field.defined_in_class,
                                             .abs_idx = base_class_field_num + field.abs_idx,
                                             .type = field.type,
                                             .def_sloc = field.def_sloc,
                                         })
                                     .value();
                });
            cl.fields = std::move(all_fields);
        }
        // Propagate methods
        auto defined_methods = cl.methods;
        auto all_methods = base_cl.methods;
        auto base_class_vtable_size = all_methods.size();
        defined_methods.for_each([&](const ast::Ident& name,
                                     const GlobalSymbols::Class::Method& method) {
            auto vtable_idx = base_class_vtable_size + method.vtable_idx;
            auto same_method = all_methods.find(name);
            if (same_method) {
                if (same_method->type != method.type) {
                    auto overriden_args_num = same_method->type.arg_types->size();
                    auto overriding_args_num = method.type.arg_types->size();
                    if (overriden_args_num != overriding_args_num) {
                        errp.error(
                                method.def_sloc,
                                "overriding method type mismatch: overridden method `", name,
                                "` has ", overriden_args_num, " argument",
                                &"s"[overriden_args_num == 1], ", but overriding method has ",
                                overriding_args_num, " argument",
                                &"s"[overriding_args_num == 1])
                            .note(
                                same_method->def_sloc,
                                "overridden method's definition is here");
                    } else {
                        errp.error(
                                method.def_sloc,
                                "overriding method type mismatch: overridden method `", name,
                                "` has type `", as_str(same_method->type),
                                "`, overriding method has type `", as_str(method.type), '`')
                            .note(
                                same_method->def_sloc,
                                "overridden method's definition is here");
                    }
                }
                vtable_idx = same_method->vtable_idx;
            }
            all_methods = all_methods.insert_or_assign(
                name,
                {
                    .defined_in_class = method.defined_in_class,
                    .vtable_idx = vtable_idx,
                    .type = method.type,
                    .def_sloc = method.def_sloc,
                });
        });
        cl.methods = std::move(all_methods);
    }

    void check_acyclicity_and_propagate_members() {
        enum State { VISITING, VISITED };
        std::map<GlobalSymbols::Class*, State> state;
        auto process = [&](auto& self, GlobalSymbols::Class& cl) -> GlobalSymbols::Class& {
            if (cl.base_class) {
                auto [state_it, inserted] = state.try_emplace(&cl, VISITING);
                if (not inserted) {
                    switch (state_it->second) {
                    case VISITED: return cl;
                    case VISITING: {
                        auto error_msg =
                            concat("class hierarchy contains a cycle: `", cl.name);
                        auto* curr = state_it->first;
                        do {
                            error_msg += " extends ";
                            error_msg += curr->base_class.value();
                            curr = &globsym.classes.at(curr->base_class.value());
                        } while (curr != state_it->first);
                        errp.error(cl.def_sloc, error_msg, '`');
                    }
                    }
                }
                auto it = globsym.classes.find(*cl.base_class);
                if (it == globsym.classes.end()) {
                    errp.error(
                        cl.def_sloc, "base class `", *cl.base_class, "` does not exist");
                }
                propagate_members(cl, self(self, it->second));
            }
            state.insert_or_assign(&cl, VISITED);
            return cl;
        };

        for (auto& [_, cl] : globsym.classes) {
            process(process, cl);
        }
    }

    void annotate_prepost_order_times() {
        std::map<GlobalSymbols::Class*, std::vector<GlobalSymbols::Class*>> graph;
        for (auto& [_, cl] : globsym.classes) {
            graph.try_emplace(&cl); // create entry, so that every class has an entry in graph
            if (cl.base_class) {
                graph[&globsym.classes.at(*cl.base_class)].emplace_back(&cl);
            }
        }
        size_t curr_time = 0;
        auto dfs = [&](auto& self, GlobalSymbols::Class* cl) -> void {
            cl->pre_post_order.in = ++curr_time;
            for (auto* neighbour : graph.at(cl)) {
                self(self, neighbour);
            }
            cl->pre_post_order.out = curr_time;
        };
        for (auto& [_, cl] : globsym.classes) {
            if (not cl.base_class) {
                // cl is the root class in some hierarchy
                dfs(dfs, &cl);
            }
        }
    }

public:
    explicit GlobalSymbolsCollector(frontend::ErrorPrinter& errp)
    : errp{errp} {}

    GlobalSymbols run(const ast::Program& prog) && {
        for (auto& top_def : prog.top_defs) {
            shallow_collect(top_def);
        }
        check_acyclicity_and_propagate_members();
        annotate_prepost_order_times();
        return std::move(globsym);
    }
};

} // namespace

namespace frontend {

ast::GlobalSymbols
collect_global_symbols(const ast::Program& prog, frontend::ErrorPrinter& errp) {
    return GlobalSymbolsCollector{errp}.run(prog);
}

} // namespace frontend
