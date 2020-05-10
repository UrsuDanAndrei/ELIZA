:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
match_rule(Tokens, UserMemory, rule(Pattern, _, _, EmoList, _)) :-
				Tokens = Pattern,
				(EmoList == []; (get_emotion(UserMemory, Emo), member(Emo, EmoList))).

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
find_matching_rules(Tokens, RulesList, UserMemory, MatchingRules) :-
	findall(Rule, member(Rule, RulesList), match_rule(Tokens, UserMemory, Rule), MatchingRules).

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

get_answers_from_rule(rule(_, Answers, _, _, _), Answers).
get_actions_from_rule(rule(_, _, Actions, _, _, _), Actions).

%% rulesList2answersActionsList([], []). 
%% rulesList2answersActionsList([H|TRulesList], R) :-
%% 	rulesList2answersActionsList(TRulesList, TAnsList),
%% 	get_answers_and_actions_from_rule(H, Pereche),
%% 	append(TAnsList, Pereche, R).

compose_list_for_min_element(_, [], []).
compose_list_for_min_element(BotMemory, [HA|TAnswers], [(HAStr, Val)|TList]) :-
	compose_list_for_min_element(BotMemory, TAnswers, TList),
	get_answer(HA, BotMemory, Val),
	unwords(HA, HAStr), !.

select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :-
	rules(KeyWords, RulesList), ord_subset(KeyWords, Tokens),
	find_matching_rules(Tokens, RulesList, UserMemory, [Rule, _]),
	get_answers_from_rule(Rule, Answers),
	compose_list_for_min_element(BotMemory, Answers, List),
	min_element(List, AnswerStr),
	words(AnswerStr, Answer),
	get_actions_from_rule(Rule, Actions).

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
handle_actions(_Actions) :- fail.


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, l a:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.
find_occurrences(_UserMemory, _Result) :- fail.

% sum(+L, -S)
sum([], 0).
sum([H|T], S) :- sum(T, S1), S is S1 + H.

keysList2tokensList([], []).
keysList2tokensList([KH|KT], [TH|TR]) :- keysList2tokensList(KT, TR), words(KH, TH).

tokensList2keysList([], []).
tokensList2keysList([TH|TT], [KH|KR]) :- tokensList2keysList(TT, KR), unwords(TH, KH).

word_included(Word, TokensList, R) :- findall(Tokens, (member(Tokens, TokensList), member(Word, Tokens)), R).

get_word_included_tokens(Word, UserMemory, WordTokensList) :- dict_keys(UserMemory, KeysList),
								 keysList2tokensList(KeysList, TokensList),
								 word_included(Word, TokensList, WordTokensList).

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
get_happy_score(UserMemory, Score) :-
	findall(Val, (happy(Word), get_word_included_tokens(Word, UserMemory, WordTokensList),
				  member(Tokens, WordTokensList), get_answer(Tokens, UserMemory, Val)), ToSum), sum(ToSum, Score).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
get_sad_score(UserMemory, Score) :-
	findall(Val, (sad(Word), get_word_included_tokens(Word, UserMemory, WordTokensList),
				  member(Tokens, WordTokensList), get_answer(Tokens, UserMemory, Val)), ToSum), sum(ToSum, Score).

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
get_emotion(UserMemory, trist) :- get_happy_score(UserMemory, HappyScore),
								  get_sad_score(UserMemory, SadScore),
								  (SadScore > HappyScore), !.
get_emotion(UserMemory, fericit) :- get_happy_score(UserMemory, HappyScore),
								  get_sad_score(UserMemory, SadScore),
								  SadScore < HappyScore, !.
get_emotion(_, neutru).

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
get_tag_score(_Tag, _UserMemory, _Score) :- fail.

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_emotion(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
get_tag(_UserMemory, _Tag) :- fail.
