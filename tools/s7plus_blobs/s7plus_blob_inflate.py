"""Inflater for the blobs a S7-1200/1500 keeps on a DB object (attributes 2449 IdentES, 2544
InterfaceDescription, 2546 LineComments): "98 00 00 02" + a zlib stream whose header has FDICT set
(78 7D + a constant 4 byte DICTID per attribute) + the Adler-32 of the plain text. The preset dictionary
is not known, so every byte copied from it is returned as ("D", e), e = distance of that byte from the END
of the dictionary; literals are ("L", byte). Use load(dump_file) on a file written by
examples/laz_s7commplus_phase2_test/s7commplus_dumpblocks_test.lpr, inflate(blob[10:-4]) to decode.
Diagnostic only (see the block download investigation)."""
import re,struct
class Bits:
    def __init__(s,data): s.d=data; s.p=0; s.b=0; s.n=0
    def get(s,n):
        while s.n<n:
            s.b|=s.d[s.p]<<s.n; s.p+=1; s.n+=8
        v=s.b&((1<<n)-1); s.b>>=n; s.n-=n; return v
    def align(s): s.b=0; s.n=0
LB=[3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258]
LE=[0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
DB=[1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577]
DE=[0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13]
def build(lengths):
    # canonical huffman: returns dict (len,code)->sym
    maxl=max(lengths) if lengths else 0
    bl=[0]*(maxl+2)
    for l in lengths:
        if l: bl[l]+=1
    code=0; nxt=[0]*(maxl+2)
    for i in range(1,maxl+1):
        code=(code+bl[i-1])<<1; nxt[i]=code
    t={}
    for s,l in enumerate(lengths):
        if l: t[(l,nxt[l])]=s; nxt[l]+=1
    return t
def sym(bs,t):
    code=0
    for l in range(1,16):
        code=(code<<1)|bs.get(1)
        if (l,code) in t: return t[(l,code)]
    raise Exception('bad code')
def inflate(data):
    """returns list of items: ('L',byte) or ('M',dist,len) in order, plus output as list where dict bytes are ('D',e) with e=distance-from-dict-end"""
    bs=Bits(data); out=[]; ops=[]
    fixed_l=build([8]*144+[9]*112+[7]*24+[8]*8); fixed_d=build([5]*30)
    while True:
        final=bs.get(1); typ=bs.get(2)
        if typ==0:
            bs.align(); ln=bs.d[bs.p]|bs.d[bs.p+1]<<8; bs.p+=4
            for i in range(ln): out.append(('L',bs.d[bs.p+i])); ops.append(('L',bs.d[bs.p+i]))
            bs.p+=ln
        else:
            if typ==1: tl,td=fixed_l,fixed_d
            else:
                hlit=bs.get(5)+257; hdist=bs.get(5)+1; hclen=bs.get(4)+4
                order=[16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]
                cl=[0]*19
                for i in range(hclen): cl[order[i]]=bs.get(3)
                tc=build(cl); lens=[]
                while len(lens)<hlit+hdist:
                    s=sym(bs,tc)
                    if s<16: lens.append(s)
                    elif s==16: lens+= [lens[-1]]*(3+bs.get(2))
                    elif s==17: lens+=[0]*(3+bs.get(3))
                    else: lens+=[0]*(11+bs.get(7))
                tl=build(lens[:hlit]); td=build(lens[hlit:])
            while True:
                s=sym(bs,tl)
                if s<256: out.append(('L',s)); ops.append(('L',s))
                elif s==256: break
                else:
                    s-=257; ln=LB[s]+bs.get(LE[s]); ds=sym(bs,td); dist=DB[ds]+bs.get(DE[ds])
                    ops.append(('M',dist,ln,len(out)))
                    for i in range(ln):
                        pos=len(out)
                        src=pos-dist
                        if src>=0: out.append(out[src])
                        else: out.append(('D',-src))   # distance from dict end
        if final: break
    return out,ops
def load(fn):
    txt=open(fn).read().split('\n'); cur=None; r={}
    for l in txt:
        m=re.match(r'attr (\d+):',l)
        if m: cur=int(m.group(1))
        s=l.strip()
        if s.startswith('bytes:') and cur in (2544,2449,4578): r[cur]=bytes.fromhex(s.split(':')[1].strip())
    return r
def show(out):
    s=''
    last=None
    for o in out:
        if o[0]=='L':
            c=o[1]; s+=chr(c) if 32<=c<127 else '\\x%02x'%c
        else: s+='«%d»'%o[1]
    return s
