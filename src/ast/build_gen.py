#!/usr/bin/env python3
import sys
import re

if len(sys.argv) != 3:
    sys.exit('Usage: ' + sys.argv[0] + ' <input_file> <output_file>')

input_file = sys.argv[1]
output_file = sys.argv[2]

with open(input_file, mode='rb') as f:
	input = [line for line in f]

def process(f, lines, line_offset, extra_indent=b'', enclosing_translate_term=None, enclosing_translate_var=None, enclosing_translate_target_enum=None):
	def try_translate(beg):
		match = re.match(rb'^( *)TRANSLATE\((\w+?)\) {\n$', lines[beg])
		if not match:
			return beg
		indent = match.group(1)
		term = match.group(2)
		var = term.lower()
		f.write(b'%b%b translate(::%b %b) {\n' % (indent, term, term, var))
		f.write(b'%b    %b res = {\n' % (indent, term))
		f.write(b'%b        .val = [this, &%b]() -> decltype(%b::val) {\n' % (indent, var, term))
		f.write(b'%b            switch (%b->kind) {\n' % (indent, var))
		# Find the end of TRANSLATE block
		end = beg + 1
		while end < len(lines):
			match = re.match(indent + rb'}\n$', lines[end])
			if not match:
				end += 1
				continue
			process(f, lines[beg + 1:end], line_offset + beg + 1, extra_indent + b'            ', term, var)
			f.write(b'%b            }\n' % indent)
			f.write(b'%b            std::abort();\n' % indent)
			f.write(b'%b        }(),\n' % (indent))
			f.write(b'%b        .sloc = {\n' % (indent))
			f.write(b'%b            .line = %b->line_number,\n' % (indent, var))
			f.write(b'%b            .column = (%b->line_number != 1) + %b->char_number,\n' % (indent, var, var))
			f.write(b'%b        },\n' % (indent))
			f.write(b'%b    };\n' % (indent))
			f.write(b'%b    return res;\n' % indent)
			f.write(b'%b}\n' % indent)
			return end + 1
		sys.exit("Error: could not find end of the TRANSLATE block that starts in line {}".format(line_offset + beg))

	def try_case(beg):
		match = re.match(rb'^( *)CASE\((\w+?)\) {\n$', lines[beg]) # Single arg CASE
		if match:
			lines[beg] = b'%bCASE(%b, %b) {\n' % (match.group(1), match.group(2), match.group(2))
		match = re.match(rb'^( *)CASE\((\w+?),\s*(\w+?)\) {\n$', lines[beg]) # Double arg CASE
		if not match:
			return beg
		indent = match.group(1)
		source_term = match.group(2)
		target_term = match.group(3)
		f.write(b'%bcase ::%b_::is_%b: {\n' % (indent + extra_indent, enclosing_translate_term, source_term))
		f.write(b'%b    #define x (%b->u.%b_)\n' % (indent + extra_indent, enclosing_translate_var, source_term.lower()))
		f.write(b'%b    return %b::%b{\n' % (indent + extra_indent, enclosing_translate_term, target_term))
		# Find the end of CASE block
		end = beg + 1
		while end < len(lines):
			match = re.match(indent + rb'}\n$', lines[end])
			if not match:
				end += 1
				continue
			process(f, lines[beg + 1:end], line_offset + beg + 1, extra_indent + b'    ')
			f.write(b'%b    };\n' % (indent + extra_indent))
			f.write(b'%b    #undef x\n' % (indent + extra_indent))
			f.write(b'%b}\n' % (indent + extra_indent))
			return end + 1
		sys.exit("Error: could not find end of the CASE block that starts in line {}".format(line_offset + beg))

	def try_empty_case(beg):
		match = re.match(rb'^( *)CASE\((\w+?)\) {}\n$', lines[beg])
		if match:
			lines[beg] = b'%bCASE(%b, %b) {}\n' % (match.group(1), match.group(2), match.group(2))
		match = re.match(rb'^( *)CASE\((\w+?),\s*(\w+?)\) {}\n$', lines[beg])
		if not match:
			return beg
		indent = match.group(1) + extra_indent
		source_term = match.group(2)
		target_term = match.group(3)
		f.write(b'%bcase ::%b_::is_%b: {\n' % (indent, enclosing_translate_term, source_term))
		if enclosing_translate_target_enum:
			f.write(b'%b    return %b::%b;\n' % (indent, enclosing_translate_target_enum, target_term))
		else:
			f.write(b'%b    return %b::%b{};\n' % (indent, enclosing_translate_term, target_term))
		f.write(b'%b}\n' % indent)
		return beg + 1

	def try_translate_to_enum(beg):
		match = re.match(rb'^( *)TRANSLATE_TO_ENUM\((\w+?),\s*(\w+?)\) {\n$', lines[beg])
		if not match:
			return beg
		indent = match.group(1)
		source_term = match.group(2)
		target_enum = match.group(3)
		var = source_term.lower()
		f.write(b'%b%b translate(::%b %b) {\n' % (indent, target_enum, source_term, var))
		f.write(b'%b    switch (%b->kind) {\n' % (indent, var))
		# Find the end of TRANSLATE_TO_ENUM block
		end = beg + 1
		while end < len(lines):
			match = re.match(indent + rb'}\n$', lines[end])
			if not match:
				end += 1
				continue
			process(f, lines[beg + 1:end], line_offset + beg + 1, extra_indent + b'    ', source_term, var, target_enum)
			f.write(b'%b    }\n' % indent)
			f.write(b'%b    std::abort();\n' % indent)
			f.write(b'%b}\n' % indent)
			return end + 1
		sys.exit("Error: could not find end of the TRANSLATE_TO_ENUM block that starts in line {}".format(line_offset + beg))

	def try_translate_no_case(beg):
		match = re.match(rb'^( *)TRANSLATE_NO_CASE\((\w+?)\) {\n$', lines[beg])
		if not match:
			return beg
		indent = match.group(1)
		term = match.group(2)
		var = term.lower()
		f.write(b'%b%b translate(::%b %b) {\n' % (indent, term, term, var))
		f.write(b'%b    auto& x = %b->u.%b_;\n' % (indent, var, term.lower()))
		f.write(b'%b    return %b{\n' % (indent, term))
		# Find the end of TRANSLATE_NO_CASE block
		end = beg + 1
		while end < len(lines):
			match = re.match(indent + rb'}\n$', lines[end])
			if not match:
				end += 1
				continue
			process(f, lines[beg + 1:end], line_offset + beg + 1, extra_indent + b'    ', term, var)
			f.write(b'%b        .sloc = {\n' % (indent))
			f.write(b'%b            .line = %b->line_number,\n' % (indent, var))
			f.write(b'%b            .column = (%b->line_number != 1) + %b->char_number,\n' % (indent, var, var))
			f.write(b'%b        },\n' % (indent))
			f.write(b'%b    };\n' % indent)
			f.write(b'%b}\n' % indent)
			return end + 1
		sys.exit("Error: could not find end of the TRANSLATE_NO_CASE block that starts in line {}".format(line_offset + beg))

	def try_translate_list(beg):
		match = re.match(rb'^( *)TRANSLATE_LIST\((\w+?)\);\n$', lines[beg])
		if not match:
			return beg
		indent = match.group(1)
		term = match.group(2)
		var = term.lower()
		f.write(b'%bstd::vector<%b> translate(::List%b list) {\n' % (indent, term, term))
		f.write(b'%b    std::vector<%b> res;\n' % (indent, term))
		f.write(b'%b    while (list) {\n' % indent)
		f.write(b'%b        res.emplace_back(translate(list->%b_));\n' % (indent,var))
		f.write(b'%b        list = list->list%b_;\n' % (indent,var))
		f.write(b'%b    }\n' % indent)
		f.write(b'%b    return res;\n' % indent)
		f.write(b'%b}\n' % indent)
		return beg + 1

	idx = 0
	while idx < len(lines):
		next_idx = idx
		for func in [try_translate, try_case, try_empty_case, try_translate_to_enum, try_translate_no_case, try_translate_list]:
			next_idx = func(idx)
			if next_idx != idx:
				assert idx < next_idx
				break
		if next_idx > idx:
			idx = next_idx
		else:
			f.write(extra_indent + lines[idx])
			idx += 1

with open(output_file, mode='wb') as f:
	process(f, input, 1)
