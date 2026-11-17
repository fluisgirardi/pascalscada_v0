#!/usr/bin/env python3

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Free Pascal backend for the open62541 datatype generator (open62541 >= 1.4).
#
# Instead of keeping a fork of the upstream generator, this script loads the
# upstream tools/generate_datatypes.py (its argument parser, CSV/BSD type parser
# and CGenerator) and only replaces the output: Pascal declarations instead of
# C headers. It must be run from the open62541 "tools" directory (the
# generate_fpc script does that).
#
# Outputs:
#   <outfile>_generated.inc  - index constants, option set constants, records/enums
# With --layout-check <prefix> it also writes
#   <prefix>.c and <prefix>.pas - programs printing SizeOf/offsetof of every
#   generated type and member; their outputs must be identical, which proves the
#   Pascal records match the C structs of the library they were built against.

import os
import sys
import ntpath

_UPSTREAM = os.path.abspath('generate_datatypes.py')
if not os.path.isfile(_UPSTREAM):
    sys.exit("run this script from the open62541 tools directory (generate_datatypes.py not found)")

with open(_UPSTREAM) as f:
    _src = f.read()
_cut = _src.find('# Execute with the command line arguments')
if _cut < 0:
    sys.exit("unexpected layout of " + _UPSTREAM)
_src = _src[:_src.rfind('\n', 0, _src.rfind('#####', 0, _cut))]

sys.path.insert(0, os.path.dirname(_UPSTREAM))
_ns = {'__name__': 'open62541_generate_datatypes', '__file__': _UPSTREAM}
exec(compile(_src, _UPSTREAM, 'exec'), _ns)

CGenerator = _ns['CGenerator']
argparser = _ns['parser']
makeCIdentifier = _ns['makeCIdentifier']
CSVBSDTypeParser = _ns['CSVBSDTypeParser']
BuiltinType = _ns['BuiltinType']
EnumerationType = _ns['EnumerationType']
OpaqueType = _ns['OpaqueType']
StructType = _ns['StructType']

argparser.add_argument('--layout-check',
                       metavar='<prefix>',
                       dest='layout_check',
                       default=None,
                       help='also write <prefix>.c and <prefix>.pas layout checkers')

# Words that can not be used as field names in {$MODE DELPHI}/{$MODE OBJFPC}
# without the & escape.
PASCAL_RESERVED = set("""
absolute and array as asm begin case class const constructor destructor dispinterface
div do downto else end except exports file finalization finally for function goto if
implementation in inherited initialization inline interface is label library mod nil
not object of on operator or out packed procedure program property raise record repeat
resourcestring set shl shr string then threadvar to try type unit until uses var while
with xor self result specialize generic otherwise
""".split())


def pas_field(name):
    ident = makeCIdentifier(name)
    if ident.lower() in PASCAL_RESERVED:
        return '&' + ident
    return ident


def plain_field(name):
    """The field name without the & escape (used in printed keys)."""
    return makeCIdentifier(name)


def member_type_name(member):
    # Members whose type is a struct without members are encoded as ExtensionObject
    if not member.member_type.members and isinstance(member.member_type, StructType):
        return "ExtensionObject"
    return member.member_type.name


class PasGenerator(CGenerator):

    def __init__(self, parser, inname, outfile, is_internal_types, namespaceMap, layout_check):
        CGenerator.__init__(self, parser, inname, outfile, is_internal_types, False, namespaceMap)
        self.layout_check = layout_check

    # ---- enum element names (same de-duplication as upstream) ----
    @staticmethod
    def enum_names(enum):
        values = list(enum.elements.items())
        names = []
        used = set()
        for index, (label, value) in enumerate(values):
            name = makeCIdentifier("UA_" + enum.name.upper() + "_" + label.upper())
            candidate = name
            suffix = index
            while candidate.upper() in used:
                candidate = name + "_" + str(suffix)
                suffix += 1
            used.add(candidate.upper())
            names.append((candidate, value))
        return names

    def typed_list(self):
        for ns in self.filtered_types:
            for i, t_name in enumerate(self.filtered_types[ns]):
                yield i, t_name, self.filtered_types[ns][t_name]

    def is_dup(self, t_name):
        return t_name in getattr(self, 'cross_ns_duplicate_types', set())

    # ---- declarations ----
    def pas_enum(self, enum):
        ident = makeCIdentifier("UA_" + enum.name)
        if enum.isOptionSet:
            return "  %s = %s;" % (ident, enum.strDataType)
        lines = ["  %s = (" % ident]
        for name, value in self.enum_names(enum):
            lines.append("    %s = %s," % (name, value))
        lines.append("    __UA_%s_FORCE32BIT = $7fffffff" % makeCIdentifier(enum.name.upper()))
        lines.append("  );")
        return "\n".join(lines)

    def pas_struct(self, struct):
        ident = makeCIdentifier("UA_" + struct.name)
        out = []
        if struct.is_union:
            sw = makeCIdentifier("UA_" + struct.name + "Switch")
            swc = makeCIdentifier("UA_" + struct.name.upper() + "SWITCH")
            out.append("  %s = (" % sw)
            out.append("    %s_NONE = 0," % swc)
            for n, m in enumerate(struct.members):
                out.append("    %s_%s = %d," % (swc, makeCIdentifier(m.name.upper()), n + 1))
            out.append("    __%s_FORCE32BIT = $7fffffff" % sw.upper())
            out.append("  );")
            out.append("  P%s = ^%s;" % (sw, sw))
        out.append("  %s = record" % ident)
        if struct.is_union:
            out.append("    switchField: UA_%sSwitch;" % makeCIdentifier(struct.name))
            out.append("    case Integer of")
        for n, member in enumerate(struct.members):
            tn = makeCIdentifier(member_type_name(member))
            fld = pas_field(member.name)
            if member.is_array:
                decl = "%sSize: size_t; %s: PUA_%s;" % (plain_field(member.name), fld, tn)
            elif member.is_optional:
                decl = "%s: PUA_%s;" % (fld, tn)
            else:
                decl = "%s: UA_%s;" % (fld, tn)
            if struct.is_union:
                out.append("      %d: (%s);" % (n, decl))
            else:
                out.append("    " + decl)
        out.append("  end;")
        return "\n".join(out)

    def pas_typedef(self, t):
        if isinstance(t, EnumerationType):
            return self.pas_enum(t)
        if isinstance(t, OpaqueType):
            return "  UA_%s = UA_%s;" % (makeCIdentifier(t.name), t.base_type)
        if isinstance(t, StructType):
            return self.pas_struct(t)
        raise RuntimeError("Type does not have an associated typedef")

    def write_definitions(self):
        self.filtered_types = self.iter_types(self.parser.types)
        out = []
        p = out.append
        up = self.parser.outname.upper()
        p("(**********************************")
        p(" * Autogenerated -- do not modify *")
        p(" * open62541 types for Free Pascal, generated from %s" % self.inname)
        p(" * by tools/generate_datatypes_fpc.py")
        p(" **********************************)")
        p("")
        total = sum(len(self.filtered_types[ns]) for ns in self.filtered_types)
        p("const")
        p("  UA_%s_COUNT = %d;" % (up, total))
        for i, t_name, t in self.typed_list():
            p("  UA_%s = %d;" % (makeCIdentifier(up + "_" + t.name.upper()), i))
        # option set bit constants
        for i, t_name, t in self.typed_list():
            if self.is_dup(t_name):
                continue
            if isinstance(t, EnumerationType) and t.isOptionSet:
                for name, value in self.enum_names(t):
                    p("  %s = %s;" % (name, value))
        p("")
        p("type")
        # forward pointer types, so records can reference each other in any order
        for i, t_name, t in self.typed_list():
            if self.is_dup(t_name) or isinstance(t, BuiltinType):
                continue
            p("  PUA_%s = ^UA_%s;" % (makeCIdentifier(t.name), makeCIdentifier(t.name)))
        p("")
        for i, t_name, t in self.typed_list():
            if self.is_dup(t_name) or isinstance(t, BuiltinType):
                continue
            if t.description:
                p("  { %s: %s }" % (t.name, t.description.replace('}', ')').replace('{', '(')))
            p(self.pas_typedef(t))
        p("")
        # The numeric NodeId of every entry of the type array: LoadOpen62541 compares them
        # with the UA_TYPES of the loaded library, so a library generated with another
        # selection of types (other indices) is refused instead of corrupting memory.
        ids = []
        for i, t_name, t in self.typed_list():
            nid = t.nodeId or ""
            if nid.startswith("ns=0;"):
                nid = nid[5:]
            ids.append(nid[2:] if nid.startswith("i=") else (nid if nid.isdigit() else "0"))
        if ids:
            p("const")
            p("  UA_%s_TYPEIDS: array[0..UA_%s_COUNT-1] of UA_UInt32 = (" % (up, up))
            for k in range(0, len(ids), 16):
                chunk = ", ".join(ids[k:k + 16])
                p("    " + chunk + ("," if k + 16 < len(ids) else ""))
            p("  );")
            p("")
        with open(self.outfile + "_generated.inc", 'w') as fh:
            fh.write("\n".join(out))
        if self.layout_check:
            self.write_layout_check(self.layout_check)

    # ---- layout checkers ----
    def write_layout_check(self, prefix):
        c = ["#include <stdio.h>", "#include <stddef.h>", "#include <open62541/types.h>",
             "int main(void) {"]
        pas = ["program %s;" % makeCIdentifier(ntpath.basename(prefix)),
               "{$MODE DELPHI}{$H+}", "uses open62541;", "begin"]
        for i, t_name, t in self.typed_list():
            if self.is_dup(t_name) or isinstance(t, BuiltinType):
                continue
            ident = "UA_" + makeCIdentifier(t.name)
            c.append('  printf("%%s %%u\\n", "%s", (unsigned)sizeof(%s));' % (ident, ident))
            pas.append("  writeln('%s ', SizeOf(%s));" % (ident, ident))
            if not isinstance(t, StructType):
                continue
            for m in t.members:
                f = plain_field(m.name)
                pf = pas_field(m.name)
                keys = []
                if m.is_array:
                    if t.is_union:
                        keys.append((f + "Size", "fields.%s.%sSize" % (f, f), f + "Size"))
                        keys.append((f, "fields.%s.%s" % (f, f), pf))
                    else:
                        keys.append((f + "Size", f + "Size", f + "Size"))
                        keys.append((f, f, pf))
                else:
                    keys.append((f, ("fields." + f) if t.is_union else f, pf))
                for key, cexpr, pexpr in keys:
                    c.append('  printf("%%s %%u\\n", "%s.%s", (unsigned)offsetof(%s, %s));'
                             % (ident, key, ident, cexpr))
                    pas.append("  writeln('%s.%s ', PtrUInt(@P%s(nil)^.%s));" % (ident, key, ident, pexpr))
        c.append("  return 0;")
        c.append("}")
        pas.append("end.")
        with open(prefix + ".c", 'w') as f:
            f.write("\n".join(c) + "\n")
        with open(prefix + ".pas", 'w') as f:
            f.write("\n".join(pas) + "\n")


args = argparser.parse_args()

outname = ntpath.basename(args.outfile)
inname = ', '.join(list(map(lambda x: ntpath.basename(x.name), args.type_bsd)))

namespaceMap = {"http://opcfoundation.org/UA/": 0}
for m in args.namespace_map:
    [idx, ns] = m.split(':', 1)
    namespaceMap[ns] = int(idx)

typeparser = CSVBSDTypeParser(args.opaque_map, args.selected_types,
                              args.no_builtin, outname, args.import_bsd,
                              args.type_bsd, args.type_csv, args.type_xml,
                              namespaceMap)
typeparser.create_types()

generator = PasGenerator(typeparser, inname, args.outfile, args.internal, namespaceMap, args.layout_check)
generator.write_definitions()
