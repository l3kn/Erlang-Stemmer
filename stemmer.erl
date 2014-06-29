-module(stemmer).
-export([stem/1, stemString/1, isVowel/1, containsVowel/1
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

-define(do_nothing_e0(W), exception0(W) -> W).
-define(replace_e0(W,R), exception0(W) -> R).

?replace_e0("skis", "ski");
?replace_e0("skies", "sky");
?replace_e0("dying", "die");
?replace_e0("lying", "lie");
?replace_e0("tying", "tie");
?replace_e0("idly", "idl");
?replace_e0("gently", "gentl");
?replace_e0("ugly", "ugli");
?replace_e0("early", "earli");
?replace_e0("only", "onli");
?replace_e0("singly", "singl");

?do_nothing_e0("sky");
?do_nothing_e0("news");
?do_nothing_e0("howe");
?do_nothing_e0("atlas");
?do_nothing_e0("cosmos");
?do_nothing_e0("bias");
?do_nothing_e0("andes");

exception0(Word) -> reverse(exception1(step0(step1A(reverse(Word))))).

-define(do_nothing_e1(W), exception1(W) -> W).

?do_nothing_e1("gninni");
?do_nothing_e1("gnituo");
?do_nothing_e1("gninnac");
?do_nothing_e1("gnirreh");
?do_nothing_e1("gnirrae");
?do_nothing_e1("deecorp");
?do_nothing_e1("deecxe");
?do_nothing_e1("deeccus");

exception1(Word) ->
  step5(step4(step3(step2(step1C(step1B(Word)))))).

%% Prepare

prepare0("'" ++ Word) -> Word;
prepare0(Word) -> Word.

prepare1("y" ++ Word) -> "Y" ++ Word;
prepare1(Word) -> re:replace(Word,"(?<=[aeiouy])y" ,"Y",[{return, list}]).

%% Step 0

-define(remove0(S), step0(S ++ Word) -> Word).

?remove0("'s'");
?remove0("s'");
?remove0("'");
step0(Word) -> Word.

%% Step 1A

-define(replace1A(S, R), step1A(S ++ Word) -> R ++ Word).

?replace1A("sess", "ss");
step1A("sei" ++ Word) -> ie_helper(Word);
step1A("dei" ++ Word) -> ie_helper(Word);
?replace1A("ss", "ss");
?replace1A("su", "su");

step1A("s" ++ ([_|Xs] = Word)) ->
  case containsVowel(Xs) of
    true -> Word;
    false -> "s" ++ Word
  end;

step1A(Word) -> Word.

ie_helper(Word) ->
    if
      length(Word) > 1 -> "i" ++ Word;
      true -> "ei" ++ Word
    end.

%% Step 1B

-define(delete1B(S), step1B(S ++ Word) -> step1B_helper(Word, S)).

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

?delete1B("de");
?delete1B("ylde");
?delete1B("gni");
?delete1B("ylgni");

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

-define(replace2(S, R), step2(S ++ Word) -> step2_helper(S, Word, R)).

?replace2("lanoita", "eta");
?replace2("lanoit", "noit");
?replace2("icne", "ecne");
?replace2("icna", "ecna");
?replace2("ilba", "elba");
?replace2("iltne", "tne");
?replace2("rezi", "ezi");
?replace2("noitazi", "ezi");
?replace2("noita", "eta");
?replace2("rota", "eta");
?replace2("msila", "la");
?replace2("itila", "la");
?replace2("illa", "la");
?replace2("ssenluf", "luf");
?replace2("ilsuo", "suo");
?replace2("ssensuo", "suo");
?replace2("ssenevi", "evi");
?replace2("itivi", "evi");
?replace2("itilib", "elb");
?replace2("ilb", "elb");
?replace2("illuf", "luf");
?replace2("ilssel", "ssel");

step2("igol" ++ Word)    -> step2_helper("igo", "l" ++ Word, "go");

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

-define(replace3(S, R), step3(S ++ Word) -> step3_helper(S, Word, R)).

?replace3("lanoita", "eta");
?replace3("lanoit", "noit");
?replace3("ezila", "la");
?replace3("etaci", "ci");
?replace3("itici", "ci");
?replace3("laci", "ci");
?replace3("luf", "");

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

-define(delete4(S), step4(S ++ Word) -> step4_helper(S, Word, "")).

?delete4("la");
?delete4("ecna");
?delete4("ecne");
?delete4("re");
?delete4("ci");
?delete4("elba");
?delete4("elbi");
?delete4("tna");
?delete4("tneme");
?delete4("tnem");
?delete4("tne");
?delete4("msi");
?delete4("eta");
?delete4("iti");
?delete4("suo");
?delete4("evi");
?delete4("ezi");

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
