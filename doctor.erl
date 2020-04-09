%% File: doctor.erl
%% Author: Ken Lambert
%% Purpose: provides domain logic for the doctor program.
%% inputs.

-module(doctor).

-export([reply/2, state/1, greeting/1, farewell/1, add_sentence/2, name/1]).

-import(myrandom, [random_int/1, probabililty/1, pick_random/1]).
-import(lists, [map/2]).
-import(maps, [find/2, get/2, from_list/1]).
-import(string, [join/2, tokens/2, to_lower/1]).

%% Create the initial state, which consists of
%% a patient's name
%% a map of replacements
%% a list of hedges
%% a list of qualifiers
%% an empty history list
state(Name) ->
    Replacements = from_list([{"i", "you"}, {"my", "your"}, {"your", "my"},
                              {"you", "I"}, {"me", "you"}]),
    Hedges = ["Please tell me more.",
              "Many of my patients tell me the same thing.",
              "I'd like to hear more about that."],
    Qualifiers = ["Why do you say that ",
                  "You seem to think that ",
                  "I'd like to hear more about why you say that "],
    from_list([{name, Name}, {replacements, Replacements}, {hedges, Hedges},
               {qualifiers, Qualifiers}, {history, []}]).

%% Access the patient's name in the state
name(State) -> get(name, State).

%% Access the replacements in the state
replacements(State) -> get(replacements, State).

%% Access the hedges in the state
hedges(State) -> get(hedges, State).

%% Access the qualifiers in the state
qualifiers(State) -> get(qualifiers, State).

%% Access the history in the state
history(State) -> get(history, State).

%% Add sentence to history in state
add_sentence(Sentence, State) ->
    NewHistory = [Sentence | history(State)],
    State#{history := NewHistory}.

%% Returns Value if not error, or DefaultValue otherwise
%% Calling form: maybe(MaybeValue, DefaultValue)
%% Example calls:
%% maybe(error, "No way!"")
%% "No way!"
%% maybe({ok, "Yes way!"})
%% "Yes way!"
maybe(DefaultValue,error) -> DefaultValue;
maybe(_DefaultValue, {ok, Value}) -> Value.

%% Returns replacement if Word is present, or Word otherwise.
my_lookup(Word, Replacements) -> maybe(Word, find(to_lower(Word),
                                                  Replacements)).

%% Switches personal pronouns in a sentence
%% Calling form: change_person(sentence)
%% Example call:
%% change_person("My mother hates me")
%% "your mother hates you"
change_person(Sentence, Replacements) ->
    join(map(fun(Word) -> my_lookup(Word, Replacements) end,
                                    tokens(Sentence, " ")), " ").

%% Replies to a sentence by selecting one of three strategies
%% Calling form: reply(sentence)
%% Example call:
%% reply("My mother hates me")
%% "Why do you say that your mother hates you?"
reply(Sentence, State) ->
    Replacements = replacements(State),
    Hedges = hedges(State),
    Qualifiers = qualifiers(State),
    History = history(State),
    PickFromTen = random_int(10),
    if
        PickFromTen > 5 -> pick_random(Qualifiers) ++
                           change_person(Sentence, Replacements);
        (PickFromTen > 2) and (length(History) > 3) ->
            "Earlier you said that " ++
            change_person(pick_random(History), Replacements);
        true -> pick_random(Hedges)
    end.

greeting(State) ->
    "Good day, " ++ name(State) ++
    ", how can I help you (type 'quit' to exit)?\n".

farewell(State) ->
    "Have a nice day, " ++ name(State) ++ "!\n".
