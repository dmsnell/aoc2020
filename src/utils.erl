-module(utils).

-export([
    count/3,
    isolated/1,
    isolated/2
]).

count(Mapper, Value, List)
    when is_function(Mapper),
         is_list(List) ->
    length([1 || Item <- List, Mapper(Item) == Value]).

%% Use this function instead to un-isolated parts
%% and get better stack trackes in the shell
% isolated(Function) -> Function().
% isolated(Function, _Timeout) -> Function().

isolated(Function) ->
    isolated(Function, 1000).

isolated(Function, Timeout) when is_function(Function) ->
    Self = self(),
    spawn(fun () -> isolated_runner(Self, Function) end),
    receive
        {ok, Response} -> Response;
        {throw, EValue} -> erlang:throw(EValue);
        {error, EValue} -> erlang:error(EValue)
    after Timeout -> erlang:error(timeout)
    end.

isolated_runner(From, Function) when is_function(Function) ->
    try Function() of
        Value -> From ! {ok, Value}
    catch
        EType:EValue -> From ! {EType, EValue}
    end.