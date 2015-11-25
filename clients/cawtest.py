#!/usr/bin/env python
from cawsock import CawTerminal
import random

def test_lsd():
    console=CawTerminal()
    ans=console.send_print("CRE asd")
    assert ans.startswith("OK")
    ans=console.send_print("LSD")
    assert ans.startswith("OK")
    ans=console.send_print("LSD")
    assert ans.startswith("OK")
    ans=console.send_print("LSD")
    assert ans.startswith("OK")



if __name__ == '__main__':
    console=CawTerminal()
    console.send_print("LSD")
