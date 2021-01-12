project('laby',
    ['c', 'cpp'],
    license: 'MIT',
    meson_version : '>=0.52.1',
    default_options : meson.get_cross_property('project_configuration', [
        'cpp_std=c++17',
        'warning_level=3',
    ])
)

cc = meson.get_compiler('c')
cpp = meson.get_compiler('cpp')

if get_option('warning_level') > '0'
    warnings = [
        '-Wshadow',
        '-Wunreachable-code',
        '-Wdocumentation',
        '-Wgnu',
        '-Wunused-lambda-capture',
        '-Wno-c++20-designator',
        '-Wno-gnu-empty-struct',
        '-Wno-extern-c-compat',
    ]
    foreach warning : warnings
        if cc.has_argument(warning)
            add_project_arguments(warning, language : 'c')
        endif
        if cpp.has_argument(warning)
            add_project_arguments(warning, language : 'cpp')
        endif
    endforeach
endif

lexer_c = custom_target('Lexer.c',
    input : 'src/ByBnfc/Latte.l',
    output : 'Lexer.c',
    command : [find_program('flex'), '-PLatte', '-o@OUTPUT@', '@INPUT@']
)
parser_c = custom_target('Parser.c',
    input : 'src/ByBnfc/Latte.y',
    output : 'Parser.c',
    command : [find_program('bison'), '-t', '-pLatte', '@INPUT@', '-o', '@OUTPUT@']
)

by_bnfc = declare_dependency(
    sources : [
        'src/ByBnfc/Absyn.c',
        lexer_c,
        parser_c,
    ],
    include_directories : 'src/ByBnfc/',
    compile_args : ['-Wno-unreachable-code', '-D_POSIX_C_SOURCE=200809L']
)

ast_build_cc = custom_target('ast_build.cc',
    input : 'src/ast/build.cc.template',
    output : 'ast_build.cc',
    command : [find_program('src/ast/build_gen.py'), '@INPUT@', '@OUTPUT@']
)

ast_build = declare_dependency(
    sources : ast_build_cc,
)

latc_x86_64 = executable('latc_x86_64',
    sources : [
        'src/backend/x86_64.cc',
        'src/frontend/global_symbols.cc',
        'src/frontend/static_analyzer.cc',
        'src/frontend/type_checker.cc',
        'src/ir/ast_to_ir.cc',
        'src/ir/eliminate_dead_code.cc',
        'src/ir/eliminate_unnecessary_phis.cc',
        'src/ir/ir_printer.cc',
        'src/ir/make_ssa.cc',
        'src/ir/optimize.cc',
        'src/ir/propagate_constants.cc',
        'src/ir/propagate_copies.cc',
        'src/ir/remove_phis.cc',
        'src/latc_x86_64.cc',
    ],
    dependencies : [
        ast_build,
        by_bnfc,
    ]
)

persistent_map_tester = executable('persistent_map_tester',
    sources : 'src/persistent_map_tester.cc',
)

all_targets = [
    latc_x86_64,
    persistent_map_tester,
]
run_target('format', command : [find_program('format.py'), meson.current_source_dir()])
run_target('tidy', command : [find_program('tidy')], depends : all_targets)
