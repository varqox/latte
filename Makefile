include src/Makefile.config

FLEX = flex
FLEX_OPTS = -PLatte

BISON = bison
BISON_OPTS = -t -pLatte

.PHONY: all
all: latc_x86_64 latc
	@printf "\033[32mBuild finished\033[0m\n"

$(eval $(call add_generated_target, src/ByBnfc/Lexer.c, \
	$$(FLEX) $$(FLEX_OPTS) -o$$@ $$<, \
	src/ByBnfc/Latte.l Makefile \
))
$(eval $(call add_generated_target, src/ByBnfc/Parser.c, \
	$$(BISON) $$(BISON_OPTS) $$< -o $$@, \
	src/ByBnfc/Latte.y Makefile \
))

$(eval $(call add_generated_target, src/ast/build.cc, \
	src/ast/build_gen.py $$< $$@, \
	src/ast/build.cc.template src/ast/build_gen.py Makefile \
))

BNFC_SRCS := \
	src/ByBnfc/Absyn.c \
	src/ByBnfc/Lexer.c \
	src/ByBnfc/Parser.c \

$(call SRCS_TO_OBJS, $(BNFC_SRCS)): override INTERNAL_EXTRA_C_FLAGS := -D_POSIX_C_SOURCE=200809L

define LATC_FLAGS
INTERNAL_EXTRA_CXX_FLAGS := -I. -Wno-extern-c-compat
endef
LATC_SOURCES := $(BNFC_SRCS) \
	src/ast/build.cc \
	src/frontend/global_symbols.cc \
	src/frontend/static_analyzer.cc \
	src/frontend/type_checker.cc \
	src/ir/ast_to_ir.cc \
	src/ir/ir_printer.cc \
	src/latc_x86_64.cc \

$(eval $(call add_executable, latc_x86_64, $(LATC_FLAGS), $(LATC_SOURCES)))
$(eval $(call add_executable, latc, $(LATC_FLAGS), $(LATC_SOURCES)))

$(eval $(call add_executable, src/persistent_map_tester,, \
	src/persistent_map_tester.cc \
))

.PHONY: format
format:
	python3 format.py .
