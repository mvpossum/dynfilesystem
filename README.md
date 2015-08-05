Introduction
================
Click and Working File System (CAWFS) es un sistema de archivos distribuidos
que requiere la minima intervencion del usuario para comenzar a funcionar.


Requeriments
================
- make
- erlang (for running the server)
- python (for both clients)
- fusepy (for fuse client, available at https://aur.archlinux.org/packages/python-fusepy-git/ for Arch Linux)
- python-dateutil (for fuse client)


Running
================
The server can be run using make, example:

> make run port=41581 folder=servidor

Available parameters are port, folder and cant. Use cant to span multiple servers in the same machine.

Default folder is server, default port is random.


