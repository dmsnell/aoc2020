-module(utils).

-export([
    count/3
]).

count(Mapper, Value, List)
    when is_function(Mapper),
         is_list(List) ->
    length([1 || Item <- List, Mapper(Item) == Value]).