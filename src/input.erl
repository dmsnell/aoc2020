-module(input).

-export([
    lines/1,
    number_lines/1,
    read/1
]).

-spec read(Name :: string()) -> Data :: binary().

read(Name) ->
    Path = filename:join([code:priv_dir(aoc2020), Name ++ ".txt"]),
    {ok, Data} = file:read_file(Path),
    Data.

-spec lines(Name :: string()) -> Lines :: list(binary()).

lines(Name) ->
    binary:split(read(Name), [<<"\r\n">>, <<"\n">>], [global]).

-spec number_lines(Name :: string()) -> Numbers :: list(integer()).

number_lines(Name) ->
    lists:map(fun binary_to_integer/1, lines(Name)).