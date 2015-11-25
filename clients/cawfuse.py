#!/usr/bin/env python
import re
from stat import S_IFDIR, S_IFREG
from sys import argv, exit
from errno import ENOENT, EIO, EPIPE, EPERM
from collections import defaultdict
from fuse import FUSE, Operations, LoggingMixIn, FuseOSError, fuse_get_context
from time import time
import dateutil.parser
from datetime import datetime
from cawsock import CawSock, discover


        
class CawFS(LoggingMixIn, Operations):
    conans=re.compile("OK ID (\d+)")
    opnans=re.compile("OK FD (\d+)")
    statans=re.compile("OK SIZE (\d+) ACCESS ([\d\-\:\s]+) MODIFY ([\d\-\:\s]+) CREATE ([\d\-\:\s]+)")
    reaans=re.compile(b'OK SIZE (\d+) ?(.*)', re.S)
    def __init__(self):
        self.s=CawSock()
        (self.ip, self.port) = discover()
        self.s.connect((self.ip, self.port))
        ans=self.s.cmdstr('CON')
        if not self.conans.match(ans):
            raise FuseOSError(IOE)
        self.root=dict(st_mode=(S_IFDIR | 0o755 ), st_ctime=time(),
                               st_mtime=time(), st_atime=time(), st_nlink=1)
    
    def readdir(self, path, fh):
        ans=self.s.cmdstr("LSD").split(' ')
        return ['.', '..'] + ans[1:]
        
    def getattr(self, path, fh=None):
        if path=='/':
            return self.root
        ans=self.s.cmdstr("STAT "+path[1:])
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
        self.s.cmd("CRE "+path[1:])
        return 0
        
    def destroy(self, path):
        self.s.cmd("BYE")
        self.s.close()

    def read(self, path, length, offset, fh):
        ans=self.s.cmd("REA2 "+path[1:]+" "+str(length)+" "+str(offset))
        ret=self.reaans.findall(ans)
        if not ret:
            raise FuseOSError(ENOENT)
        return ret[0][1]
        
    def rename(self, old, new):
        self.s.cmdstr("MV "+old[1:]+" "+new[1:])
    
    def unlink(self, path):
        self.s.cmdstr("DEL "+path[1:])
    
    def write(self, path, data, offset, fh):
        self.s.cmd("CRE "+path[1:])
        self.s.cmd(bytearray("WRT2 "+path[1:]+" "+str(len(data))+" "+str(offset)+" ", "ascii")+data)
        return len(data)
    
if __name__ == '__main__':
    if len(argv) != 2:
        print('usage: %s <mountpoint>' % argv[0])
        exit(1)
    fuse = FUSE(CawFS(), argv[1], foreground=True)
    
        
