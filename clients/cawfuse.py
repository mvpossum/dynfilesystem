#!/usr/bin/env python
import re
from stat import S_IFDIR, S_IFREG
from sys import argv, exit
from errno import *
from collections import defaultdict
from fuse import FUSE, Operations, LoggingMixIn, FuseOSError, fuse_get_context
from time import time
import dateutil.parser
from datetime import datetime
from cawsock import CawTerminal


        
class CawFS(LoggingMixIn, Operations):
    ok=re.compile("OK")
    opnans=re.compile("OK FD (\d+)")
    statans=re.compile("OK SIZE (\d+) ACCESS ([\d\-\:\s]+) MODIFY ([\d\-\:\s]+) CREATE ([\d\-\:\s]+)")
    reaans=re.compile(b'OK SIZE (\d+) ?(.*)', re.S)
    tryagain=re.compile('ERROR 9 .*', re.S)
    def __init__(self):
        self.openfiles = {}
        try:
            self.s=CawTerminal()
        except OSError:
            raise FuseOSError(IOE)
        self.root=dict(st_mode=(S_IFDIR | 0o755 ), st_ctime=time(),
                               st_mtime=time(), st_atime=time(), st_nlink=1)
        #~ self.release("hola",  self.create("hola", 0))
        
    def send(self, cmd):
        try:
            ans=self.s.send_print(cmd)
            if self.tryagain.findall(ans):
                raise FuseOSError(EAGAIN)
            return ans
        except OSError(ECONNRESET):
            raise FuseOSError(IOE)

    def readdir(self, path, fh):
        ans=self.send("LSD").split(' ')
        return ['.', '..'] + ans[1:]
        
    def getattr(self, path, fh=None):
        if path=='/':
            return self.root
        ans=self.send("STAT "+path[1:])
        data=self.statans.findall(ans)
        if not data:
            raise FuseOSError(ENOENT)
        uid, gid, pid = fuse_get_context()
        (tam, atime, mtime, ctime)=data[0]
        #~ parse = lambda dt: dateutil.parser.parse(dt).timestamp() #only for python >=3.3 :/
        parse = lambda dt: (dateutil.parser.parse(dt)- datetime(1970, 1, 1)).total_seconds()
        return dict(st_mode=(S_IFREG | 0o755), st_nlink=1,
                                st_size=int(tam), st_ctime=parse(ctime), st_mtime=parse(mtime),
                                st_atime=parse(atime), st_uid=uid, st_gid=gid)
                                
    def create(self, path, mode):
        self.send("CRE "+path[1:])
        return self.open(path, mode)

    def open(self, path, flags):
        ret = self.opnans.findall(self.send("OPN "+path[1:]))
        if not ret:
            raise FuseOSError(ETXTBSY)
        self.openfiles[path]=int(ret[0])
        return int(ret[0])
        
    def destroy(self, path):
        self.send("BYE")
        self.s.close()

    def read(self, path, length, offset, fh):
        
        ans=self.s.send_bin("REA2 FD "+str(fh)+" OFFSET "+str(offset)+" SIZE "+str(length))
        ret=self.reaans.findall(ans)
        if not ret:
            raise FuseOSError(ENOENT)
        return (ret[0][1])
        
    def write(self, path, data, offset, fh):
        ans=self.s.send_bin(bytearray("WRT2 FD "+str(fh)+" OFFSET "+str(offset)+" SIZE "+str(len(data))+" ", "ascii")+data)
        return len(data)
        
    def rename(self, old, new):
        if self.openfiles.has_key(old):
            self.release(old, self.openfiles[old])
        if self.openfiles.has_key(new):
            self.release(old, self.openfiles[new])
        self.send("MV "+old[1:]+" "+new[1:])
    
    def unlink(self, path):
        self.send("DEL "+path[1:])
        
    def release(self, path, fh):
        if self.openfiles.has_key(path):
            del self.openfiles[path]
            self.send("CLO FD "+str(fh))
    
    
if __name__ == '__main__':
    if len(argv) != 2:
        print('usage: %s <mountpoint>' % argv[0])
        exit(1)
    fuse = FUSE(CawFS(), argv[1], foreground=True)
    
        
