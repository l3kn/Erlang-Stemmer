-module(stemmer).
-export([stem/1, stemString/1, isVowel/1, containsVowel/1
        ,r1/1, r2/1, endsShort/1]).

-import(lists, [reverse/1, member/2, map/2]).

vowels()  -> "aeiouy".
doubles() -> "bdfgmnprt".

stemString(String) ->
  Words = string:tokens(string:to_lower(String), " "),
  Stems = map(fun(X) -> stem(X) end, Words),
  string:join(Stems, " ").

stem(Word) -> reverse(step2(
                      step1C(
                      step1B(
                      step1A(
                      step0(reverse(Word))))))).

%% Step 0

step0("'s'" ++ Word) -> Word;
step0("s'" ++ Word) -> Word;
step0("'" ++ Word) -> Word;
step0(Word) -> Word.

%% Step 1A

step1A("sess" ++ Word) -> Word;
step1A("sei" ++ Word) -> step1A_ie(Word);
step1A("dei" ++ Word) -> step1A_ie(Word);
step1A("ss" ++ Word) -> "ss" ++ Word;
step1A("su" ++ Word) -> "su" ++ Word;

step1A("s" ++ ([_|Xs] = Word)) ->
  case containsVowel(Xs) of
    true -> Word;
    false -> "s" ++ Word
  end;

step1A(Word) -> Word.

step1A_ie(Word) ->
    if
      length(Word) > 1 -> "i" ++ Word;
      true -> "ei" ++ Word
    end.

%% Step 1B
step1B(("dee" ++ Word) = Full) ->
  case re:run(rev1(Full), "[A-Za-z]dee") of
    nomatch -> "dee" ++ Word;
    _       -> "ee"  ++ Word
  end;

step1B(("yldee" ++ Word) = Full) ->
  case re:run(rev1(Full), "[A-Za-z]yldee") of
    nomatch -> "yldee" ++ Word;
    _       -> "ee" ++ Word
  end;

step1B("de" ++ Word)    -> step1B_helper(Word, "de");
step1B("ylde" ++ Word)  -> step1B_helper(Word, "ylde");
step1B("gni" ++ Word)   -> step1B_helper(Word, "gni");
step1B("ylgni" ++ Word) -> step1B_helper(Word, "ylgni");

step1B(Word) -> Word.

step1B_helper(Word, A) ->
  case containsVowel(Word) of
    true  -> step1B2(Word);
    false -> A ++ Word
  end.


step1B2([X1,X2|Xs] = Word) ->
  End1  = member(([X1] ++ [X2]), ["ta", "lb", "zi"]),
  End2  = (X1 =:= X2) and member(X1, doubles()),
  Short = isShortR(Word),

  if
    End1 -> "e" ++ Word;
    End2 -> [X2] ++ [Xs];
    Short -> "e" ++ Word;
    true -> Word
  end.


%% Step 1C

step1C("y" ++ Word) ->
  case step1C_y(Word) of
    true -> "i" ++ Word;
    false -> "y" ++ Word
  end;

step1C("Y" ++ Word) ->
  case step1C_y(Word) of
    true -> "i" ++ Word;
    false -> "Y" ++ Word
  end;

step1C(Word) -> Word.

step1C_y([X|Xs]) ->
  V = isVowel(X),
  if
    (not V) and (Xs =/= []) -> true;
    true              -> false
  end.

%% Step 2

step2("lanoit" ++ Word)  -> step2_helper("lanoit", Word, "noit");
step2("icne" ++ Word)    -> step2_helper("icne", Word, "ecne");
step2("icna" ++ Word)    -> step2_helper("icna", Word, "ecna");
step2("ilba" ++ Word)    -> step2_helper("ilba", Word, "elba");
step2("iltne" ++ Word)   -> step2_helper("iltne", Word, "tne");
step2("rezi" ++ Word)    -> step2_helper("rezi", Word, "ezi");
step2("noitazi" ++ Word) -> step2_helper("noitazi", Word, "ezi");
step2("lanoita" ++ Word) -> step2_helper("lanoita", Word, "eta");
step2("noita" ++ Word)   -> step2_helper("noita", Word, "eta");
step2("rota" ++ Word)    -> step2_helper("rota", Word, "eta");
step2("msila" ++ Word)   -> step2_helper("msila", Word, "la");
step2("itila" ++ Word)   -> step2_helper("itila", Word, "la");
step2("illa" ++ Word)    -> step2_helper("illa", Word, "la");
step2("ssenluf" ++ Word) -> step2_helper("ssenluf", Word, "luf");
step2("ilsuo" ++ Word)   -> step2_helper("ilsuo", Word, "suo");
step2("ssensuo" ++ Word) -> step2_helper("ssensuo", Word, "suo");
step2("ssenevi" ++ Word) -> step2_helper("ssenevi", Word, "evi");
step2("itivi" ++ Word)   -> step2_helper("itivi", Word, "evi");
step2("itilib" ++ Word)  -> step2_helper("itilib", Word, "elb");
step2("ilb" ++ Word)     -> step2_helper("ilb", Word, "elb");
step2("igol" ++ Word)    -> step2_helper("igol", Word, "gol");
step2("illuf" ++ Word)   -> step2_helper("illuf", Word, "luf");
step2("ilssel" ++ Word)  -> step2_helper("ilssel", Word, "ssel");

step2("il" ++ ([X|Xs] = Word)) -> 
  case member(X, "cdeghkmnrt") of
    true -> Word;
    _    -> "il" ++ Word
  end;

step2(Word) -> Word.

step2_helper(Suffix, Rest, Replace) ->
  case re:run(rev1(Suffix ++ Rest), Suffix) of
    nomatch -> Suffix  ++ Rest;
    _       -> Replace ++ Rest
  end.

%% TODO: Step3

%% Helper Functions

rev1(Word) -> reverse(r1(reverse(Word))).
rev2(Word) -> reverse(r2(reverse(Word))).

r1([X1,X2|Xs]) ->
  V1 = isVowel(X1),
  V2 = isVowel(X2),
  if
    V1 and not V2 -> Xs;
    true          -> r1([X2|Xs])
  end;

r1([]) -> [].

r2(Word) -> r1(r1(Word)).

endsShort(Word) -> endsShortR(reverse(Word)).

isShortR(Word) -> (rev1(Word) =:= []) and endsShortR(Word).

endsShortR([X1,X2]) -> (not isVowel(X1)) and isVowel(X2);
endsShortR([X1,X2,X3|_]) ->
  (not member(X1, "aeiouyYwx")) and
  member(X2, "aeiouy") and
  (not member(X3, "aeiouy"));
endsShortR(_) -> false.

isVowel(Char) -> member(Char, vowels()).

containsVowel([X|Xs]) ->
  case isVowel(X) of
    true  -> true;
    false -> containsVowel(Xs)
  end;

containsVowel([]) -> false.
