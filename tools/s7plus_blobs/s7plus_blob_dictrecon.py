"""Partial reconstruction of the preset dictionary of the 2544 InterfaceDescription blob from a folder of
dumps made by s7commplus_dumpblocks_test: a dictionary byte is accepted when at least 3 DIFFERENT names (DB
or member names, known from the dump) agree on it and nothing disagrees. Usage:
  python3 s7plus_blob_dictrecon.py <dump folder> [out.json]   (out.json: {distance-from-dict-end: char})
The dictionary is a template document (an XML with encoding="utf-16", BlockInterface, VersionElement ...);
what this recovers is only the part that names reveal, not the boilerplate. Diagnostic only."""
import sys,glob,collections,re,json,time,os; sys.path.insert(0,os.path.dirname(os.path.abspath(__file__)))
from s7plus_blob_inflate import *
DUMPDIR=sys.argv[1] if len(sys.argv)>1 else '.'
OUT=sys.argv[2] if len(sys.argv)>2 else 'dic2544_partial.json'
def parse(fn):
    L=open(fn).read().split('\n')
    m=re.match(r'DB (\d+) "(.*)" relation',L[0])
    names=set()
    if m: names.add(m.group(2))
    for l in L:
        mm=re.match(r'var (.*) type=',l)
        if mm:
            for part in mm.group(1).split('.'):
                if len(part)>=3 and not re.match(r'^DB\d+$',part): names.add(part)
    return names
samples=[]
for fn in sorted(glob.glob(os.path.join(DUMPDIR,'DB*.txt'))):
    d=load(fn)
    if 2544 not in d: continue
    try: out,ops=inflate(d[2544][10:-4])
    except Exception: continue
    if len(out)>30000: continue
    nm=sorted(parse(fn),key=len,reverse=True)[:60]
    samples.append((fn,out,nm))
print('samples',len(samples),flush=True)
dic={}
for it in range(8):
    t=time.time()
    votes=collections.defaultdict(lambda: collections.defaultdict(set))
    for fn,out,names in samples:
        s=''.join(chr(o[1]) if o[0]=='L' else (dic.get(o[1],'\x00')) for o in out)
        for S in names:
            pat=re.compile('(?=('+''.join('[%s\x00]'%re.escape(c) if c not in '\\]^-' else '[\\%s\x00]'%c for c in S)+'))')
            for m in pat.finditer(s):
                i=m.start(); seg=s[i:i+len(S)]
                known=sum(1 for a,b in zip(seg,S) if a==b)
                if known<max(4,int(len(S)*0.75)) or known==len(S): continue
                for k,ch in enumerate(seg):
                    if ch=='\x00': votes[out[i+k][1]][S[k]].add(S)
    new=0
    for u,c in votes.items():
        if u in dic: continue
        good=[ch for ch,names in c.items() if len(names)>=3]
        if len(good)==1 and len(c)==1:
            dic[u]=good[0]; new+=1
    print('iter',it,'new',new,'total',len(dic),'%.0fs'%(time.time()-t),flush=True)
    if new==0: break
tot=0;res=0
for fn,out,names in samples:
    for o in out:
        if o[0]=='D':
            tot+=1; res+= o[1] in dic
print('dict bytes referenced',tot,'resolved',res,'%.1f%%'%(100*res/tot))
json.dump({str(k):v for k,v in dic.items()},open(OUT,'w'))
