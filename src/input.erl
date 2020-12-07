-module(input).

-export([
    groups_and_lines/1,
    lines/1,
    number_list/1,
    raw/1
]).

-spec groups_and_lines(Name :: string()) -> list(list(binary())).

groups_and_lines(Name) ->
    Groups = binary:split(raw(Name), [<<"\n\n">>], [global]),
    [[binary_to_list(Form) || Form <- binary:split(Group, [<<"\n">>], [global])] || Group <- Groups].

-spec raw(Name :: string()) -> Data :: binary().

raw(Name) ->
    Path = filename:join([code:priv_dir(aoc2020), Name ++ ".txt"]),
    {ok, Data} = file:read_file(Path),
    Data.

-spec lines(Name :: string()) -> Lines :: list(binary()).

lines(Name) ->
    binary:split(raw(Name), [<<"\r\n">>, <<"\n">>], [global]).

-spec number_list(Name :: string()) -> Numbers :: list(integer()).

number_list(Name) ->
    lists:map(fun binary_to_integer/1, lines(Name)).
