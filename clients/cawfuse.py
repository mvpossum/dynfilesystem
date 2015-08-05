#!/usr/bin/env python
import re
import socket
from threading import Lock
from stat import S_IFDIR, S_IFREG
import struct
from sys import argv, exit
from errno import ENOENT, EIO, EPIPE, EPERM
from collections import defaultdict
from fuse import FUSE, Operations, LoggingMixIn, FuseOSError, fuse_get_context
from time import time
import dateutil.parser
from datetime import datetime


def discover(Share):
    multicast_group = '224.0.0.251'
    server_address = (multicast_group, 41581)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    group = socket.inet_aton(multicast_group)
    mreq = struct.pack('4sL', group, socket.INADDR_ANY)
    sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
    sock.bind(server_address)
    udpre=re.compile("SERVER "+Share+" \d+ ([\d.]+) (\d+)")
    while True:
        data = sock.recv(1024)
        ans=udpre.findall(str(data))
        if ans:
            (ip, port)=ans[0]
            return (ip, int(port)) 

class CawSock:
    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.lock = Lock()
    
    def connect(self, share='DEFAULT'):
        self.sock.connect(discover(share))

    def raw_send(self, msg):
        tosend=struct.pack('>I', len(msg))+msg
        totalsent = 0
        while totalsent < len(tosend):
            sent = self.sock.send(tosend[totalsent:])
            if sent == 0:
                raise FuseOSError(EPIPE)
            totalsent = totalsent + sent
            
    def raw_recv(self, tam):
        chunks = []
        bytes_recd = 0
        while bytes_recd < tam:
            chunk = self.sock.recv(min(tam - bytes_recd, 32768))
            if chunk == b'':
                raise FuseOSError(EPIPE)
            chunks.append(chunk)
            bytes_recd = bytes_recd + len(chunk)
        return b''.join(chunks)
        
    def receive(self):
        (tam,)=struct.unpack('>I', self.raw_recv(4))
        return self.raw_recv(tam)
    
    def cmd(self, msg):
        if isinstance(msg, str):
            msg=bytearray(msg, 'ascii')
        with self.lock:
            self.raw_send(msg)
            return self.receive()
            
    def cmdstr(self, msg):
        return self.cmd(msg).decode()
        
    def close(self):
        self.sock.close()
        
class CAWFS(LoggingMixIn, Operations):
    conans=re.compile("OK ID (\d+)")
    opnans=re.compile("OK FD (\d+)")
    statans=re.compile("OK SIZE (\d+) ACCESS ([\d\-\:\s]+) MODIFY ([\d\-\:\s]+) CREATE ([\d\-\:\s]+)")
    reaans=re.compile(b'OK SIZE (\d+) ?(.*)', re.S)
    def __init__(self):
        self.s=CawSock()
        self.s.connect()
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
    fuse = FUSE(CAWFS(), argv[1], foreground=True)
    
        
