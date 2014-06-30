-module(stemmer).
-export([stem/1, stemString/1, r1/1, r2/1]).

-import(lists, [reverse/1, member/2, map/2]).
-import(string, [tokens/2, to_lower/1]).

stemString(String) ->
  Words = tokens(to_lower(String), " "),
  Stems = map(fun(X) -> stem(X) end, Words),
  string:join(Stems, " ").

-define(do_nothing(W), stem(W) -> W).
-define(replace(W,R), stem(W) -> R).

?replace("skis", "ski");
?replace("skies", "sky");
?replace("dying", "die");
?replace("lying", "lie");
?replace("tying", "tie");
?replace("idly", "idl");
?replace("gently", "gentl");
?replace("ugly", "ugli");
?replace("early", "earli");
?replace("only", "onli");
?replace("singly", "singl");

?do_nothing("sky");
?do_nothing("news");
?do_nothing("howe");
?do_nothing("atlas");
?do_nothing("cosmos");
?do_nothing("bias");
?do_nothing("andes");

stem(Word) when length(Word) > 2 ->
  Low = to_lower(Word),
  Pre = pre1(pre0(Low)),
  Rev = reverse(Pre),
  W1  = step1A(step0(Rev)),
  Ex  = ["gninni", "gnituo", "gninnac" ,"gnirreh"
        ,"gnirrae", "deecorp", "deecxe", "deeccus"],
  W2 = case member(W1, Ex) of 
    true -> W1;
    _    -> step5(step4(step3(step2(step1C(step1B(W1))))))
  end,
  to_lower(reverse(W2));

stem(Word) -> Word.

%% Prepare

pre0("'" ++ Word) -> Word;
pre0(Word) -> Word.

pre1("y" ++ Word) -> "Y" ++ Word;
pre1(Word) -> re:replace(Word,"(?<=[aeiouy])y" ,"Y",[{return, list}]).


%% Step 0
%%
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

step1B("dee" ++ Word) -> r1_helper(Word, "dee", "dee", "ee");
step1B("yldee" ++ Word) -> r1_helper(Word, "yldee", "yldee", "ee");

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
  End2  = (X1 =:= X2) and isDouble(X1),
  Short = isShortR(Word),
  if
    End1 or Short -> "e" ++ Word;
    End2 -> [X2] ++ Xs;
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

-define(replace2(S, R), step2(S ++ Word) -> r1_helper(Word, S, S, R)).

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

step2("igol" ++ Word) -> r1_helper("l" ++ Word, "igo", "igo", "go");

step2("il" ++ ([X|_Xs] = Word)) -> 
  case isLiEnding(X) of
    true -> r1_helper(Word, "il", "il", "");
    _    -> "il" ++ Word
  end;

step2(Word) -> Word.

%% Step3

-define(replace3(S, R), step3(S ++ Word) -> r1_helper(Word, S, S, R)).

?replace3("lanoita", "eta");
?replace3("lanoit", "noit");
?replace3("ezila", "la");
?replace3("etaci", "ci");
?replace3("itici", "ci");
?replace3("laci", "ci");
?replace3("luf", "");

step3("ssen" ++ Word)     -> r1_helper(Word, "ssen", "ssen", "");
step3("evita" ++ Word)    -> r2_helper(Word, "evita", "evita", "");

step3(Word) -> Word.

r2_helper(Word, S, Rep1, Rep2) ->
  case inR2(S ++ Word, S) of
    true  -> Rep2 ++ Word;
    false -> Rep1 ++ Word
  end.

r1_helper(Word, S, Rep1, Rep2) ->
  case inR1(S ++ Word, S) of
    true  -> Rep2 ++ Word;
    false -> Rep1 ++ Word
  end.

%% Step4

-define(delete4(S), step4(S ++ Word) -> r2_helper(Word, S, S, "")).

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

step4("nois" ++ Word) -> r2_helper("s" ++ Word, "noi", "noi", "");
step4("noit" ++ Word) -> r2_helper("t" ++ Word, "noi", "noi", "");

step4(Word) -> Word.

%% Step 5

step5("e" ++ Word) ->
  R1 = inR1("e" ++ Word, "e"),
  R2 = inR2("e" ++ Word, "e"),
  Sh = endsShortR(Word),
  if
    R2 or (R1 and not Sh) -> Word;
    true                  -> "e" ++ Word
  end;

step5("ll" ++ Word) -> r2_helper("l" ++ Word, "l", "l", "");
step5(Word) -> Word.

%% Helper Functions

isDouble(C)    -> member(C, "bdfgmnprt").
isLiEnding(C)  -> member(C, "cdeghkmnrt").
isVowel(C)     -> member(C, "aeiouy").
isVowelExt(C)  -> member(C, "aeiouyYwx").
isConsonant(C) -> not isVowel(C).

inR1(Word, String) -> includes(rev1(Word), String).
inR2(Word, String) -> includes(rev2(Word), String).

includes(Word, String) ->
  case re:run(Word, String) of
    nomatch -> false;
    _       -> true
  end.

rev1(Word) -> reverse(r1(reverse(Word))).
rev2(Word) -> reverse(r2(reverse(Word))).

r1("gener" ++ Word) -> Word;
r1("commun" ++ Word) -> Word;
r1("arsen" ++ Word) -> Word;
r1(Word) -> r1_(Word).

r1_([X1,X2|Xs]) ->
  case isVowel(X1) and isConsonant(X2) of
    true -> Xs;
    _    -> r1_([X2|Xs])
  end;

r1_(_) -> "".

r2(Word) -> r1_(r1(Word)).

isShortR(Word) -> (rev1(Word) =:= []) and endsShortR(Word).

endsShortR([X1,X2]) -> isConsonant(X1) and isVowel(X2);
endsShortR([X1,X2,X3|_]) ->
  (not isVowelExt(X1)) and isVowel(X2) and isConsonant(X3);
endsShortR(_) -> false.

containsVowel([X|Xs]) ->
  case isVowel(X) of
    true -> true;
    _    -> containsVowel(Xs)
  end;

containsVowel([]) -> false.
