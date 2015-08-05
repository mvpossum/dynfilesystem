-module(cmd).
-include("logging.hrl").
-export([parse/1, make/1, flat/1, toBin/2]).

%Por cada comando indico el tipo de sus argumentos
%%Comandos del anillo
info("SERVER") -> [string,int,ip,int];
info("WORK") -> [int];
info("SETPRV") -> [ip,int];
info("OKPRV") -> [];
info("OKWORK") -> [];
info("IMNXT") -> [];

%%Comandos Internos
info("FS") -> [int, int, string, term];

%%Comandos del cliente
info("CON") -> [];%OJO
info("LSD") -> [];
info("STAT") -> [string];
info("DEL") -> [string];
info("CRE") -> [string];
info("OPN") -> [string];
info("WRT") -> [string, int, string, int, binary];
info("WRT2") -> [string, int, int, binary];
info("REA") -> [string, int, string, int];
info("REA2") -> [string, int, int];
info("MV") -> [string, string];
info("CLO") -> [string, int];
info("BYE") -> [].

fromBin(B, binary) -> B;
fromBin(B, string) -> binary_to_list(B);
fromBin(B, int) -> list_to_integer(binary_to_list(B));
fromBin(B, ip) -> {ok, Ip}=inet_parse:address(binary_to_list(B)), Ip;
fromBin(B, term) -> binary_to_term(B).
    
toBin(X, binary) when is_binary(X) -> X;
toBin(X, string) when is_list(X) -> list_to_binary(X);
toBin(X, int) -> list_to_binary(io_lib:format("~p", [X]));
toBin(X, ip) -> list_to_binary(inet_parse:ntoa(X));
toBin(X, term) -> term_to_binary(X).

make([H],[HT], Acc) -> [Acc, toBin(H, HT)];
make([H|T], [HT|TT], Acc) -> make(T, TT, [Acc, toBin(H, HT),<<" ">>]).
make([H|T]) -> make([H|T], [string|info(H)], <<>>).

parse(<<>>, [], Acc) -> Acc;
parse(B, [H], Acc) -> Acc++[fromBin(B, H)];
parse(B, [H|T], Acc) ->
    {P,_}=binary:match(B, <<" ">>),
    parse(binary:part(B, P+1, byte_size(B)-P-1), T, Acc++[fromBin(binary:part(B, 0, P), H)]).
parse(B) ->
    try
        case binary:match(B, <<" ">>) of
        {P,_} ->
            Name=binary_to_list(binary:part(B, 0, P)),
            parse(binary:part(B, P+1, byte_size(B)-P-1), info(Name), [Name]);
        nomatch -> 
            Name=binary_to_list(B),
            case info(Name) of [] -> [Name]; _ -> error end
        end
    catch _:_ -> error
    end.

flat(M) ->
    F = fun(A, B) -> <<A/binary, B/binary>> end,
    lists:foldr(F, <<>>, lists:flatten(M)).
