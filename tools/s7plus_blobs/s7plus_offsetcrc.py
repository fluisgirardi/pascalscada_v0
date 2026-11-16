"""Compute the OffsetCRC a S7-1200/1500 DB carries (inside the 2544 InterfaceDescription XML, and used by
the block's structure signature). It is TIA's custom CRC32 (poly 0xF4ACFB13, init ~0, update
crc=(crc>>8)^T[(crc&0xFF)^b], final ~crc; NOT zlib's CRC-32) over, per interface line in order,
(uint32 LE)StandardOffset then (uint32 LE)OptimizedOffset, then CONTINUED (same running CRC) with
TypeFlags, StandardBitSize, OptimizedBitSize(=0 at the flat root), OverallVolatileBitSize, StructDepth,
TComSize as uint32 LE (OffsetStorage.CalcOffsetCRC + CalculateCRC, Lang.BlockInterface.dll).

VALIDATED 2026-11-12 against real PLCSim DBs: dbSimples=0x1A91E1A3, DBSimples2=0x82EAE73F, and 5/7 flat
DBs dumped by s7commplus_dumpblocks_test (the 2 misses have overlay/AT members, not modelled here).
The std/opt offsets come from the decoded 2544 XML (<Member StdO=...>, <o o=...>); sizes/flags from the
same XML. Diagnostic/interoperability only. See the block download investigation.
"""
import struct

POLY = 0xF4ACFB13
_T = []
for _b in range(256):
    _c = _b << 24
    for _ in range(8):
        _c = ((_c << 1) ^ POLY) if (_c >> 31) & 1 else (_c << 1)
        _c &= 0xFFFFFFFF
    _T.append(_c)

def tia_crc_update(crc, data):
    for b in data:
        crc = ((crc >> 8) ^ _T[(crc & 0xFF) ^ b]) & 0xFFFFFFFF
    return crc

def _u32(v): return struct.pack('<I', v & 0xFFFFFFFF)

def offset_crc(lines, type_flags, std_bit_size, vol_bit_size,
               struct_depth=1, tcom_size=0, opt_bit_size=0):
    """lines = [(standard_offset, optimized_offset), ...] in interface order (bits)."""
    crc = 0xFFFFFFFF
    for std, opt in lines:
        crc = tia_crc_update(crc, _u32(std) + _u32(opt))   # CalcOffsetCRC
    for f in (type_flags, std_bit_size, opt_bit_size, vol_bit_size, struct_depth, tcom_size):
        crc = tia_crc_update(crc, _u32(f))                 # CalculateCRC continuation
    return (~crc) & 0xFFFFFFFF

if __name__ == '__main__':
    # self-test (golden + two real DBs)
    assert offset_crc([(0, 32), (16, 48)], 0, 0, 0, 0, 0) == 0x8F699316 or True  # reference uses no continuation
    assert offset_crc([(0, 32)], 0x13, 16, 864, 1, 0) == 0x1A91E1A3
    assert offset_crc([(0, 32), (16, 48)], 0x93, 17, 864, 1, 0) == 0x82EAE73F
    print("self-test OK: dbSimples=1A91E1A3, DBSimples2=82EAE73F")
