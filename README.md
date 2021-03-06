Introduction
================
Click and Working File System (CAWFS) is a distributed file system which requires the minimum user intervention to work correctly.

Requirements
================
- make
- erlang (for running the server, working with version Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] )
- python2 (for running clients)
- python2-fusepy (for fuse client, available at https://aur.archlinux.org/packages/python2-fusepy/ for Arch Linux)
- python2-dateutil (for fuse client)


> sudo apt-get install fuse erlang

> pip install python-dateutil fusepy

Running
================
The server can be run using make, example:

> make run port=41581 folder=servidor

Available parameters are port, folder and cant. Use cant to span multiple servers in the same machine:

> make run cant=4

Default folder is server, default port is random.

To run the telnet:

> make telnet

To run the tests (the server must be running)

> make test

To mount it (using FUSE):

> make fuse mount=client

Documentation
================
A short report (informe/informe.pdf) in Spanish explains the main algorithms implemented.  
