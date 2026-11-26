"""Decode the compressed compiler blobs on a S7-1200/1500 DB object (attributes 2449 IdentES,
2544 InterfaceDescription, 2546 LineComments, 2533 BodyDescription, 2545 CompilerSwitches) using
the zlib preset dictionaries exported from TIA Portal's PLInternal.dll by the companion app's
"Dicionarios zlib" tab (<dict folder>/Dictionary_*.zdict).

The blob is `98 00 00 02` (V12-with-dictionary header) + a zlib stream (78 7D + 4-byte DICTID +
deflate + Adler-32). The right dictionary is the one whose Adler-32 equals the DICTID; this script
picks it automatically. Diagnostic/interoperability only (reading our own DB's engineering data).

Usage: python3 s7plus_blob_decode.py <dict folder> <dump file from s7commplus_dumpblocks_test> [attr]
"""
import zlib, sys, os, re, glob

ATTR_HINT = {2449:'IdentES', 2544:'InterfaceDescription', 2546:'LineComments',
             2533:'BodyDescription', 2545:'CompilerSwitches'}

def load_dump(fn):
    txt = open(fn).read().split('\n'); cur=None; out={}
    for l in txt:
        m = re.match(r'attr (\d+):', l)
        if m: cur=int(m.group(1))
        s=l.strip()
        if s.startswith('bytes:') and cur is not None:
            out.setdefault(cur, bytes.fromhex(s.split(':')[1].strip()))
    return out

def load_dicts(folder):
    d={}
    for f in glob.glob(os.path.join(folder,'*.zdict')):
        b=open(f,'rb').read(); d[zlib.adler32(b)&0xffffffff]=(os.path.basename(f),b)
    return d

def decode(blob, dicts):
    if len(blob)<8 or blob[:4]!=b'\x98\x00\x00\x02': return None,'not a V12 dictionary blob'
    dictid=int.from_bytes(blob[6:10],'big')  # 78 7D <DICTID BE>
    if dictid not in dicts: return None,f'dictionary {dictid:08X} not found in folder'
    name,zd=dicts[dictid]
    o=zlib.decompressobj(zdict=zd)
    try: out=o.decompress(blob[4:])+o.flush()
    except Exception as e: return None,f'inflate failed ({name}): {e}'
    return out, name

if __name__=='__main__':
    if len(sys.argv)<3:
        print(__doc__); sys.exit(1)
    dicts=load_dicts(sys.argv[1]); dump=load_dump(sys.argv[2])
    only=int(sys.argv[3]) if len(sys.argv)>3 else None
    for aid in sorted(dump):
        if aid not in ATTR_HINT: continue
        if only and aid!=only: continue
        b=dump[aid]
        if b[:4]!=b'\x98\x00\x00\x02': continue
        text,info=decode(b,dicts)
        print(f'=== attr {aid} {ATTR_HINT[aid]} ({len(b)} B compressed, dict {info}) ===')
        print(text.decode('utf-8','replace') if text else f'[{info}]'); print()
