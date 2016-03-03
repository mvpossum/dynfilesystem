-module(fsstate).
-include("logging.hrl").
-include("fsstate.hrl").
-include_lib("kernel/include/file.hrl").
-export([create/2,  update_lockeds/2,
        is_locked/2, who_locked/2, why_locked/2, lock_file/4, unlock_file/2,
        list_local_files/1, is_open_locally/2,
        delete_file/2, exists_locally/2, file_info/2, create_file/2, open_file/2, close_file/2, write_file/3, write_file/4, read_file/3, read_file/4, rename_file/3,
        open_file_client/2, close_file_client/2, is_open_client/2]).

create(Id, Folder) ->
    file:make_dir(Folder),
    #fsstate{id=Id, folder=Folder, openlocalfiles=#{}, lockedfiles=#{}, clientfiles=sets:new()}.

update_lockeds(Lockeds, St) ->
    St#fsstate{lockedfiles = Lockeds}.
is_locked(File, St) ->
    maps:is_key(File, St#fsstate.lockedfiles).   
who_locked(File, St) -> {Who, _} = maps:get(File, St#fsstate.lockedfiles, {notlocked, notlocked}), Who.
why_locked(File, St) -> {_, Why} = maps:get(File, St#fsstate.lockedfiles, {notlocked, notlocked}), Why.
lock_file(File, Who, Why, St) ->
    St#fsstate{lockedfiles = (St#fsstate.lockedfiles)#{File => {Who, Why}}}. 
unlock_file(File, St) ->
    St#fsstate{lockedfiles = maps:remove(File, St#fsstate.lockedfiles)}.
    
list_local_files(St) ->
    {ok, List} = file:list_dir(St#fsstate.folder),
    [File || File <- List, filelib:is_file(filename:join(St#fsstate.folder, File))].
is_open_locally(File, St) ->
    maps:is_key(File, St#fsstate.openlocalfiles).
    
delete_file(File, St) ->
    file:delete(filename:join(St#fsstate.folder, File)).
    
exists_locally(File, St) ->
    filelib:is_file(filename:join(St#fsstate.folder, File)).

file_info(File, St) ->
    case file:read_file_info(filename:join(St#fsstate.folder, File)) of
    {ok, FileInfo} -> {FileInfo#file_info.size, FileInfo#file_info.atime, FileInfo#file_info.mtime, FileInfo#file_info.ctime};
    {error, enoent} -> notfound
    end.
    
create_file(File, St) ->
    Init = case File of
				"caro" -> <<" _      __     __\n| \\    / _\\   / _\\\n \\ \\   ||     ||\n /  \\  ||__   ||__\n/_/\\_\\ \\__/ O \\__/\n">>;
				_ -> <<>>
           end,
    ok=file:write_file(filename:join(St#fsstate.folder, File), Init).

open_file(File, St) ->
    case file:open(filename:join(St#fsstate.folder, File), [read,append,raw,binary]) of
    {ok, FD} -> St#fsstate{openlocalfiles = (St#fsstate.openlocalfiles)#{File => FD}};
    _ -> exit("Can't open file.")
    end.
close_file(File, St) ->
    file:close(maps:get(File, St#fsstate.openlocalfiles)),
    St#fsstate{openlocalfiles = maps:remove(File, St#fsstate.openlocalfiles) }.
write_file(File, Data, St) ->
    case file:write(maps:get(File, St#fsstate.openlocalfiles), Data) of
    ok -> ok;
    {error, Reason} -> exit(?ERROR("Can't write file ~p: ~p", [File, Reason]))
    end.
write_file(File, Offset, Data, St) ->
    FD = maps:get(File, St#fsstate.openlocalfiles),
    file:position(FD, Offset),
    case file:write(FD, Data) of
    ok -> ok;
    {error, Reason} -> exit(?ERROR("Can't write file ~p: ~p", [File, Reason]))
    end.
read_file(File, Size, St) ->
    case file:read(maps:get(File, St#fsstate.openlocalfiles), Size) of
    {ok, Data} -> Data;
    eof -> <<>>;
    {error, Reason} -> exit(?ERROR("Can't read file ~p: ~p", [File, Reason]))
    end.
read_file(File, Offset, Size, St) ->
    FD = maps:get(File, St#fsstate.openlocalfiles),
    file:position(FD, Offset),
    case file:read(FD, Size) of
    {ok, Data} -> Data;
    eof -> <<>>;
    {error, Reason} -> exit(?ERROR("Can't read file ~p: ~p", [File, Reason]))
    end.
rename_file(File, NewName, St) ->
    case file:rename(filename:join(St#fsstate.folder, File), filename:join(St#fsstate.folder, NewName)) of
    ok -> ok;
    {error, Reason} -> exit(?ERROR("Can't rename file ~p to ~p: ~p", [File, NewName, Reason]))
    end.
    
open_file_client(File, St) ->
    St#fsstate{clientfiles = sets:add_element(File, St#fsstate.clientfiles)}.
close_file_client(File, St) ->
    St#fsstate{clientfiles = sets:del_element(File, St#fsstate.clientfiles)}.
is_open_client(File, St) ->
    sets:is_element(File, St#fsstate.clientfiles). 

