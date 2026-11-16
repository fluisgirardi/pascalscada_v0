"""Port of the TIA memory-layout signature (attribute 7733 StructureModified / 2581 for FB/FC), the
8-byte fingerprint in a block, from the Rust reference (siemens-automation-portal/app/src/hashing.rs,
compute_layout). VALIDATED against the reference golden vector (a DB with Valor Int 0/0 + Ligado Bool
16/16, areas [4,0,0], optimized -> 8126067641626254379).

IMPORTANT (checked live 2026-11-11 on PLCSim .4): this is the BASE Fingerprint mode. Real DBs created in
recent TIA on a "Plus" CPU use FingerprintExtended, which adds undocumented data to the stream, so this
does NOT reproduce the 7733 of dbSimples/DBSimples2 read from the PLC even with correct inputs. Diagnostic
only; see the block download investigation."""

import hashlib, struct
def g(t):
    b=bytes.fromhex(t.replace('-','')); return b[:4][::-1]+b[4:6][::-1]+b[6:8][::-1]+b[8:]
AB=g("4D7FA920-B8F2-4998-8CAE-E54DB560C0A0"); AE=g("22DE02DE-00E0-452E-B14F-71B81E24D020")
STATIC=g("8CC19674-29C8-4CA6-8438-7592728035E6"); SEC_END=g("59A8EFF7-4C25-494C-89B9-2CD6A31CFC8A")
LINE_END=g("3B9B8DFD-E491-4522-86D5-C77C5EE185E7"); CONT_END=g("D902801B-6718-4076-8326-25056C0328DF")
def i32(v): return struct.pack('<i',v)
# member: (name, tc, opt, std, remanence)
def layout(members, areas, optimized):
    s=AB+i32(areas[0])+i32(areas[1])+i32(areas[2])+AE   # A) memory sizes
    # B) BFS. Root: emit Static section guid + SEC_END, push Section. member_idx=[] -> then CONT_END
    s+=STATIC+SEC_END
    s+=CONT_END                                          # end of Root
    for (name,tc,oo,so,rem) in members:                  # Section(Static) members
        b=name.encode('utf-8')+b'\0'
        # is_array_or_string? tc in {16,19,62} or type_name nonempty -> none here
        b+=i32(tc)
        area = rem if optimized else 1
        # not (Return && Void)
        b+=i32(area)+i32(oo)+i32(so)
        b+=LINE_END
        s+=b
    s+=CONT_END                                          # end of Section(Static)
    return s
def val(s): return struct.unpack('<Q',hashlib.sha1(s).digest()[:8])[0]
gold=[('Valor',5,0,0,3),('Ligado',1,16,16,3)]
v=val(layout(gold,(4,0,0),True))
print('gold got', v, 'want 8126067641626254379', v==8126067641626254379)
print('stream hex', layout(gold,(4,0,0),True).hex())

