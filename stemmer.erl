-module(stemmer).
-export([stem/1, stemDebug/1, stemString/1, isVowel/1, containsVowel/1
        ,r1/1, r2/1, endsShort/1, isShortR/1, testDiffs/0, getDiffs/0]).

-import(lists, [filter/2, sum/1, reverse/1, member/2, map/2]).

getDiffs() ->
  {ok, Binary} = file:read_file("diffs.csv"),
  Lines = string:tokens(binary_to_list(Binary), "\n"),
  Pairs = map(fun(X) -> string:tokens(X, ",") end, Lines),
  filter(fun(X) -> testDiff(X) =:= 1 end, Pairs).

testDiffs() ->
  {ok, Binary} = file:read_file("diffs.csv"), %% TODO: Exceptions
  Lines = string:tokens(binary_to_list(Binary), "\n"),
  Pairs = map(fun(X) -> string:tokens(X, ",") end, Lines),
  sum(map(fun(X) -> testDiff(X) end, Pairs)) / length(Lines) * 100.

testDiff([From, To]) -> 
  case stem(From) of
    To -> 0;
    _  -> 1
  end.

vowels()  -> "aeiouy".
doubles() -> "bdfgmnprt".

stemString(String) ->
  Words = string:tokens(string:to_lower(String), " "),
  Stems = map(fun(X) -> stem(X) end, Words),
  string:join(Stems, " ").

stem(Word) when length(Word) < 3 -> Word;
stem(Word) -> string:to_lower(exception0(
              prepare1(prepare0(string:to_lower(Word))))).

exception0("skis") -> "ski";
exception0("skies") -> "sky";
exception0("dying") -> "die";
exception0("lying") -> "lie";
exception0("tying") -> "tie";
exception0("idly") -> "idl";
exception0("gently") -> "gentl";
exception0("ugly") -> "ugli";
exception0("early") -> "earli";
exception0("only") -> "onli";
exception0("singly") -> "singl";

exception0(W="sky") -> W;
exception0(W="news") -> W;
exception0(W="howe") -> W;
exception0(W="atlas") -> W;
exception0(W="cosmos") -> W;
exception0(W="bias") -> W;
exception0(W="andes") -> W;

exception0(Word) -> reverse(exception1(step0(step1A(reverse(Word))))).

exception1(W="gninni") -> W;
exception1(W="gnituo") -> W;
exception1(W="gninnac") -> W;
exception1(W="gnirreh") -> W;
exception1(W="gnirrae") -> W;
exception1(W="deecorp") -> W;
exception1(W="deecxe") -> W;
exception1(W="deeccus") -> W;

exception1(Word) ->
  step5(step4(step3(step2(step1C(step1B(Word)))))).

stemDebug(Word) ->
  io:fwrite(Word).

%% Prepare

prepare0("'" ++ Word) -> Word;
prepare0(Word) -> Word.

prepare1("y" ++ Word) -> "Y" ++ Word;
prepare1(Word) -> re:replace(Word,"(?<=[aeiouy])y" ,"Y",[{return, list}]).

%% Step 0

step0("'s'" ++ Word) -> Word;
step0("s'" ++ Word) -> Word;
step0("'" ++ Word) -> Word;
step0(Word) -> Word.

%% Step 1A

step1A("sess" ++ Word) -> "ss" ++ Word;
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
  case re:run(rev1(Full), "dee") of
    nomatch -> "dee" ++ Word;
    _       -> "ee"  ++ Word
  end;

step1B(("yldee" ++ Word) = Full) ->
  case re:run(rev1(Full), "yldee") of
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
    End2 -> [X2] ++ Xs;
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
  end;

step1C_y(_) -> false.

%% Step 2

step2("lanoita" ++ Word) -> step2_helper("lanoita", Word, "eta");
step2("lanoit" ++ Word)  -> step2_helper("lanoit", Word, "noit");
step2("icne" ++ Word)    -> step2_helper("icne", Word, "ecne");
step2("icna" ++ Word)    -> step2_helper("icna", Word, "ecna");
step2("ilba" ++ Word)    -> step2_helper("ilba", Word, "elba");
step2("iltne" ++ Word)   -> step2_helper("iltne", Word, "tne");
step2("rezi" ++ Word)    -> step2_helper("rezi", Word, "ezi");
step2("noitazi" ++ Word) -> step2_helper("noitazi", Word, "ezi");
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
step2("igol" ++ Word)    -> step2_helper("igo", "l" ++ Word, "go");
step2("illuf" ++ Word)   -> step2_helper("illuf", Word, "luf");
step2("ilssel" ++ Word)  -> step2_helper("ilssel", Word, "ssel");

step2("il" ++ ([X|_Xs] = Word)) -> 
  case member(X, "cdeghkmnrt") of
    true -> step2_helper("il", Word, "");
    _    -> "il" ++ Word
  end;

step2(Word) -> Word.

step2_helper(Suffix, Rest, Replace) ->
  case re:run(rev1(Suffix ++ Rest), Suffix) of
    nomatch -> Suffix  ++ Rest;
    _       -> Replace ++ Rest
  end.

%% Step3

step3("lanoita" ++ Word)  -> step3_helper("lanoita", Word, "eta");
step3("lanoit" ++ Word)   -> step3_helper("lanoit", Word, "noit");
step3("ezila" ++ Word)    -> step3_helper("ezila", Word, "la");
step3("etaci" ++ Word)    -> step3_helper("etaci", Word, "ci");
step3("itici" ++ Word)    -> step3_helper("itici", Word, "ci");
step3("laci" ++ Word)     -> step3_helper("laci", Word, "ci");
step3("luf" ++ Word)      -> step3_helper("luf", Word, "");
step3("ssen" ++ Word)     -> step3_helper("ssen", Word, "");
step3("evita" ++ Word)    ->
  case re:run(rev2("evita" ++ Word), "evita") of
    nomatch -> "evita" ++ Word;
    _       -> ""      ++ Word
  end;

step3(Word) -> Word.

step3_helper(Suffix, Rest, Replace) ->
  case re:run(rev1(Suffix ++ Rest), Suffix) of
    nomatch -> Suffix  ++ Rest;
    _       -> Replace ++ Rest
  end.

%% Step4

step4("la" ++ Word) -> step4_helper("la", Word, "");
step4("ecna" ++ Word) -> step4_helper("ecna", Word, "");
step4("ecne" ++ Word) -> step4_helper("ecne", Word, "");
step4("re" ++ Word) -> step4_helper("re", Word, "");
step4("ci" ++ Word) -> step4_helper("ci", Word, "");
step4("elba" ++ Word) -> step4_helper("elba", Word, "");
step4("elbi" ++ Word) -> step4_helper("elbi", Word, "");
step4("tna" ++ Word) -> step4_helper("tna", Word, "");
step4("tneme" ++ Word) -> step4_helper("tneme", Word, "");
step4("tnem" ++ Word) -> step4_helper("tnem", Word, "");
step4("tne" ++ Word) -> step4_helper("tne", Word, "");
step4("msi" ++ Word) -> step4_helper("msi", Word, "");
step4("eta" ++ Word) -> step4_helper("eta", Word, "");
step4("iti" ++ Word) -> step4_helper("iti", Word, "");
step4("suo" ++ Word) -> step4_helper("suo", Word, "");
step4("evi" ++ Word) -> step4_helper("evi", Word, "");
step4("ezi" ++ Word) -> step4_helper("ezi", Word, "");

step4("nois" ++ Word) ->
  case re:run(rev2("nois" ++ Word), "noi") of
    nomatch -> "nois"  ++ Word;
    _       ->  "s"    ++ Word
  end;
step4("noit" ++ Word) ->
  case re:run(rev2("noit" ++ Word), "noi") of
    nomatch -> "noit"  ++ Word;
    _       -> "t"     ++ Word
  end;

step4(Word) -> Word.

step4_helper(Suffix, Word, Replace) ->
  case re:run(rev2(Suffix ++ Word), Suffix) of
    nomatch -> Suffix  ++ Word;
    _       -> Replace ++ Word
  end.

%% Step 5

step5("e" ++ Word) ->
  R1 = inR1("e" ++ Word, "e"),
  R2 = inR2("e" ++ Word, "e"),
  Sh = endsShortR(Word),
  if
    R2 or (R1 and not Sh) -> Word;
    true                  -> "e" ++ Word
  end;

step5("ll" ++ Word) ->
  R1 = inR2("ll" ++ Word, "l"),
  if
    R1   -> "l" ++ Word;
    true -> "ll" ++ Word
  end;


step5(Word) -> Word.

%% Helper Functions

inR1(Word, String) ->
  R1 = rev1(Word),
  case re:run(R1, String) of
    nomatch -> false;
    _       -> true %%not ((R1 =:= String) or (R1 =:= [String]))
  end.

inR2(Word, String) ->
  R2 = rev2(Word),
  case re:run(R2, String) of
    nomatch -> false;
    _       -> true %%not((R2 =:= String) or (R2 =:= [String]))
  end.

rev1(Word) -> reverse(r1(reverse(Word))).
rev2(Word) -> reverse(r2(reverse(Word))).

r1("gener" ++ Word) -> Word;
r1("commun" ++ Word) -> Word;
r1("arsen" ++ Word) ->
  Word;

r1(Word) -> r1_(Word).

r1_([X1,X2|Xs]) ->
  V1 = isVowel(X1),
  V2 = isVowel(X2),
  if
    V1 and not V2 -> Xs;
    true          -> r1_([X2|Xs])
  end;

r1_(_) -> [].

r2(Word) -> r1_(r1(Word)).

endsShort(Word) -> endsShortR(reverse(Word)).

isShortR(Word) -> (rev1(Word) =:= []) and endsShortR(Word).

endsShortR([X1,X2]) -> (not isVowel(X1)) and isVowel(X2);
endsShortR([X1,X2,X3|_]) ->
  (not member(X1, "aeiouyYwx")) and
  member(X2, "aeiouy") and
  (not member(X3, "aeiouy"));
endsShortR(_) -> false.

isVowel(Char) -> member(Char, vowels()).

containsVowel(Word) ->
  case re:run(Word, "[aeuoiy]") of
    nomatch -> false;
    _       -> true
  end.
