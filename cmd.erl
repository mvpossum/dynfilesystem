-module(cmd).
-export([parse/1, make/1, concat/1]).

%Por cada comando indico el tipo de sus argumentos
info("SERVER") -> [string,int,ip,int];
info("WORK") -> [int];
info("SETPRV") -> [ip,int];
info("OKPRV") -> [];
info("OKWORK") -> [];
info("NWORK") -> [int,int].

parse(B, [binary]) -> [B];
parse(B, [string]) -> [binary_to_list(B)];
parse(B, [int]) -> [list_to_integer(binary_to_list(B))];
parse(B, [ip]) -> {ok, Ip}=inet_parse:address(binary_to_list(B)), [Ip];
parse(B, [H|T]) ->
    {P,_}=binary:match(B, [<<" ">>]),
    parse(binary:part(B, 0, P), [H]) ++ parse(binary:part(B, P+1, byte_size(B)-P-1), T).
parse(B) ->
    case binary:match(B, [<<" ">>]) of
    {P,_} ->
        Name=binary_to_list(binary:part(B, 0, P)),
        try
            [Name | parse(binary:part(B, P+1, byte_size(B)-P-1), info(Name))]
        catch _:_ -> error
        end;
    nomatch -> 
        Name=binary_to_list(B),
        try
            case info(Name) of [] -> [Name]; _ -> error end
        catch _:_ -> error
        end
    end.
    
toBin(X) when is_binary(X) -> X;
toBin(X) when is_list(X) -> list_to_binary(X);
toBin({A,B,C,D}) -> list_to_binary(inet_parse:ntoa({A,B,C,D}));%IP
toBin(X) -> list_to_binary(io_lib:format("~p", [X])).
make([H]) -> [toBin(H)];
make([H|T]) -> [toBin(H),<<" ">>,make(T)].

concat(M) ->
    F = fun(A, B) -> <<A/binary, B/binary>> end,
    lists:foldr(F, <<>>, lists:flatten(M)).
    
