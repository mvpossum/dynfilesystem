-module(fsstate).
-include("logging.hrl").
-include("fsstate.hrl").
-include_lib("kernel/include/file.hrl").
-export([create/2, list_local_files/1, is_open_locally/2,
        is_locked/2, who_locked/2, why_locked/2, lock_file/4, unlock_file/2,
        delete_file/2, exists_locally/2, file_info/2, create_file/2, open_file/2,
        open_file_client/2, close_file_client/2]).

create(Id, Folder) ->
    file:make_dir(Folder),
    #fsstate{id=Id, folder=Folder, openlocalfiles=#{}, lockedfiles=#{}, clientfiles=sets:new()}.


list_local_files(St) ->
    {ok, List} = file:list_dir(St#fsstate.folder),
    [File || File <- List, filelib:is_file(filename:join(St#fsstate.folder, File))].
is_open_locally(File, St) ->
    maps:is_key(File, St#fsstate.openlocalfiles).
    
is_locked(File, St) ->
    maps:is_key(File, St#fsstate.lockedfiles).   
who_locked(File, St) -> {Who, _} = maps:get(File, St#fsstate.lockedfiles, {undefined, notlocked}), Who.
why_locked(File, St) -> {_, Why} = maps:get(File, St#fsstate.lockedfiles, {undefined, notlocked}), Why.
lock_file(File, Who, Why, St) ->
    St#fsstate{lockedfiles = (St#fsstate.lockedfiles)#{File => {Who, Why}}}. 
unlock_file(File, St) ->
    St#fsstate{lockedfiles = maps:remove(File, St#fsstate.lockedfiles)}.
    
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
    ok=file:write_file(filename:join(St#fsstate.folder, File),<<>>).

open_file(File, St) ->
    case file:open(filename:join(St#fsstate.folder, File), [read,append,raw,binary]) of
    {ok, FD} -> St#fsstate{openlocalfiles = (St#fsstate.openlocalfiles)#{File => FD}};
    _ -> exit("Can't open file.")
    end.
    
    
open_file_client(File, St) ->
    St#fsstate{clientfiles = sets:add_element(File, St#fsstate.clientfiles)}.
close_file_client(File, St) ->
    St#fsstate{clientfiles = sets:del_element(File, St#fsstate.clientfiles)}. 

