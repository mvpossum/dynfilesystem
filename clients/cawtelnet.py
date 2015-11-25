#!/usr/bin/env python
from cawsock import CawTerminal

if __name__ == '__main__':
    console=CawTerminal()
    console.send_print("LSD")
    qry=""
    while qry != "BYE":
        qry=raw_input()
        print(console.send(qry))
    
        
