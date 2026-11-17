# fpopen62541

fpopen62541 is a Free Pascal / Lazarus binding for the [open62541](https://github.com/open62541/open62541)
OPC UA client and server library, version **1.5** (tested with 1.5.10).

It's licensed under the Mozilla Public License v2.0 (MPLv2).

> open62541 1.4 changed the client/server configuration (EventLoop, certificate groups, loggers)
> and the type array: this version of the binding does not work with 1.2/1.3 libraries.

## Using it

The library is loaded at run time (`LoadOpen62541` / `UnloadOpen62541`), so a program that uses
the unit only needs `libopen62541.so` / `libopen62541.dll` when it actually calls it.
`LoadOpen62541` checks the library it loads:

- every entry of `UA_TYPES` must have the NodeId this binding was generated for (a library built
  with another selection of types is refused instead of corrupting memory);
- the encryption support must match the `UA_ENABLE_ENCRYPTION` define of `open62541.pas`
  (`UA_ClientConfig` has more fields when the library is built with encryption).

```pascal
LoadOpen62541();          // or LoadOpen62541('/path/to/libopen62541.so')
client := UA_Client_new();
UA_ClientConfig_setDefault(UA_Client_getConfig(client));
if UA_Client_connect(client, 'opc.tcp://localhost:4840') = UA_STATUSCODE_GOOD then ...
```

Notes for Free Pascal code calling the library:

- **Floating point exceptions**: FPC unmasks them and C code does not expect them. Mask them
  (`SetExceptionMask`) around the calls to the library in the threads that make them, or a
  division by zero inside the library becomes a Pascal exception unwinding through C (and
  leaves its EventLoop marked as running).
- **Never let an exception leave a callback** (logger, data change, method...).
- **`{$mode delphi}`**: `@x` on a procedure variable is its value, not its address. Pass arrays of
  callbacks as `Pointer(MyArray)`, not `@MyArray[0]`.
- The log callback receives the C `va_list` as a pointer: format it with `UA_FormatLogMessage`.

## Building the library

```sh
git clone --branch v1.5.10 --recurse-submodules https://github.com/open62541/open62541.git
cmake -S open62541 -B open62541/build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON \
      -DUA_ENABLE_ENCRYPTION=OPENSSL
cmake --build open62541/build -j
```

The GitHub Actions workflow `.github/workflows/BuildOpen62541v1.5.yml` (run it manually, it accepts
the open62541 tag) builds these libraries for Linux (x86_64/i386, on Ubuntu 22.04) and Windows
(x64/Win32, OpenSSL linked statically from vcpkg), Release and Debug. On Linux x86_64 it also runs
`tools/check_layout` and loads the libraries with `tools/load_test.pas`.

Keep the default type selection (`UA_ENABLE_DATATYPES_ALL=ON`) and `UA_ENABLE_TYPEDESCRIPTION=ON`.
Without `UA_ENABLE_ENCRYPTION`, undefine `UA_ENABLE_ENCRYPTION` in `open62541.pas`.

## Regenerating the include files

`statuscodes.inc`, `nodeids.inc` and `types_generated.inc` are generated from the open62541 sources:

```sh
tools/generate_fpc <open62541-source-dir> .
```

`tools/generate_datatypes_fpc.py` loads the upstream `tools/generate_datatypes.py` and only
replaces its output with Pascal, so it follows the upstream generator.

## Checking the memory layout

`tools/check_layout` compiles a C and a Pascal program that print `SizeOf`/offset of every
generated type and member, and of the records written by hand (`tools/layout_handwritten.txt`),
and compares their output:

```sh
tools/check_layout <open62541-source-dir> <open62541-build-dir> <path-to-fpc>
# gentypes: 1722 sizes/offsets identical
# handtypes: 122 sizes/offsets identical
```

Run it after changing a record and on every new open62541 version / target.

## Status

- Client and the basic server API: bound and checked (`check_layout` on x86_64 Linux).
- `TestOpcUa` (the demo) still uses the 1.3 API and does not compile yet.
