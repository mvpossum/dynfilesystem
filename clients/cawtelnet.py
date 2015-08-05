#!/usr/bin/env python
import socket
import re
from threading import Lock
import struct
from sys import argv, exit

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
        
class CawTelnet:
    def __init__(self):
        self.s=CawSock()
        self.s.connect()
        print("Conected!")
    
    def interact(self):
        print("CON")
        print(self.s.cmdstr("CON"))
        while True:
            qry=input()
            print(self.s.cmdstr(qry))
            if qry=="BYE":
                break

if __name__ == '__main__':
    if len(argv) != 1:
        print('usage: %s' % argv[0])
        exit(1)
    console=CawTelnet()
    console.interact()
    
        
