-module(stemmer_tests).

-include_lib("eunit/include/eunit.hrl").
-import(lists, [filter/2, member/2, map/2]).

stemmer_test() ->
  {ok, Binary} = file:read_file("diffs.csv"),
  Lines = string:tokens(binary_to_list(Binary), "\n"),
  Pairs = map(fun(X) -> string:tokens(X, ",") end, Lines),
  ?assert_equal([], filter(fun(X) -> testDiff(X) end, Pairs)).

testDiff([From, To]) -> 
  case stemmer:stem(From) of
    To -> true;
    _  -> false
  end.

