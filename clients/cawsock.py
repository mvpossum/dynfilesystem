import struct
import socket
import re
from errno import *
from threading import Lock

def discover():
    multicast_group = '224.0.0.251'
    server_address = (multicast_group, 41581)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    group = socket.inet_aton(multicast_group)
    mreq = struct.pack('4sL', group, socket.INADDR_ANY)
    sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
    sock.bind(server_address)
    udpre=re.compile("SERVER \d+ ([\d.]+) (\d+)")
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
    
    def connect(self, (ip, port)):
        self.sock.connect((ip, port))

    def raw_send(self, msg):
        tosend=struct.pack('>I', len(msg))+msg
        totalsent = 0
        while totalsent < len(tosend):
            sent = self.sock.send(tosend[totalsent:])
            if sent == 0:
                raise OSError(errnoECONNRESET)
            totalsent = totalsent + sent
            
    def raw_recv(self, tam):
        chunks = []
        bytes_recd = 0
        while bytes_recd < tam:
            chunk = self.sock.recv(min(tam - bytes_recd, 32768))
            if chunk == b'':
                raise OSError(ECONNRESET)
            chunks.append(chunk)
            bytes_recd = bytes_recd + len(chunk)
        return b''.join(chunks)
        
    def receive(self):
        (tam,)=struct.unpack('>I', self.raw_recv(4))
        return self.raw_recv(tam)
    
    def cmdbin(self, msg):
        if isinstance(msg, str):
            msg=bytearray(msg, 'ascii')
        with self.lock:
            self.raw_send(msg)
            return self.receive()
            
    def cmdstr(self, msg):
        return self.cmdbin(msg).decode()
        
    def close(self):
        self.sock.close()
        
class CawTerminal:
    conans=re.compile("OK ID (\d+)")
    def __init__(self):
        self.lock = Lock()
        self.s=CawSock()
        (self.ip, self.port) = discover()
        self.s.connect((self.ip, self.port))
        print("Connected at %s in port %s!" % (self.ip, self.port))
        ans=self.send_print("CON")
        if not self.conans.match(ans):
            raise OSError(ECONNREFUSED)
    
    def send(self, cmd):
        return self.s.cmdstr(cmd)
        
    def send_bin(self, cmd):
        return self.s.cmdbin(cmd)
    
    def send_print(self, cmd):
        with self.lock:
            print(cmd)
            ans=self.send(cmd)
            print(ans)
            return ans
