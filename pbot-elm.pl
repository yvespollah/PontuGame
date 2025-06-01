:- use_module(library(lists)).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*        PRODUIRE_REPONSE(L_Mots,L_strings) :                           */
/*                                                                       */
/*        Input : une liste de mots L_Mots representant la question      */
/*                de l'utilisateur                                       */
/*        Output : une liste de strings donnant la                       */
/*                 reponse fournie par le bot                            */
/*                                                                       */
/*        NB Par défaut le predicat retourne dans tous les cas           */
/*            [  "Je ne sais pas.", "Les étudiants",                     */
/*               "vont m'aider, vous le verrez !" ]                      */
/*                                                                       */
/*        Je ne doute pas que ce sera le cas ! Et vous souhaite autant   */
/*        d'amusement a coder le predicat que j'ai eu a ecrire           */
/*        cet enonce et ce squelette de solution !                       */
/*                                                                       */
/* --------------------------------------------------------------------- */


produire_reponse([fin],L1) :-
    L1 = [merci, de, m, '\'', avoir, consulte], !.

produire_reponse(L,Rep) :-
    mclef(M,_), member(M,L),
    clause(regle_rep(M,_,Pattern,Rep),Body),
    match_pattern(Pattern,L), 
    call(Body), !.

produire_reponse(_,[S1,S2]) :-
    S1 = "Je ne sais pas. ",
    S2 = "Les étudiants vont m'aider, vous le verrez".

match_pattern(Pattern,Lmots) :-
    sublist(Pattern,Lmots).

match_pattern(LPatterns,Lmots) :-
    match_pattern_dist([100|LPatterns],Lmots).

match_pattern_dist([],_).
match_pattern_dist([N,Pattern|Lpatterns],Lmots) :-
    within_dist(N,Pattern,Lmots,Lmots_rem),
    match_pattern_dist(Lpatterns,Lmots_rem).

within_dist(_,Pattern,Lmots,Lmots_rem) :-
    prefixrem(Pattern,Lmots,Lmots_rem).
within_dist(N,Pattern,[_|Lmots],Lmots_rem) :-
    N > 1, Naux is N-1,
    within_dist(Naux,Pattern,Lmots,Lmots_rem).

sublist(SL,L) :-
    prefix(SL,L), !.
sublist(SL,[_|T]) :- sublist(SL,T).

sublistrem(SL,L,Lr) :-
    prefixrem(SL,L,Lr), !.
sublistrem(SL,[_|T],Lr) :- sublistrem(SL,T,Lr).

prefixrem([],L,L).
prefixrem([H|T],[H|L],Lr) :- prefixrem(T,L,Lr).



% ----------------------------------------------------------------%

nb_lutins(4).
nb_equipes(4).

mclef(commence,10).
mclef(equipe,5).
mclef(quipe,5).
mclef(deplacer,8).
mclef(deplac,8).
mclef(retirer,7).
mclef(pont,6).
mclef(regle,9).
mclef(gagner,9).
mclef(plateau,7).
mclef(ordre,8).
mclef(lutin,5).
mclef(fin,10).
mclef(bonjour,5).
mclef(merci,5).
mclef(ia,7).
mclef(intelligence,7).
mclef(artificielle,7).
mclef(jouer,8).
mclef(elimine,8).
mclef(couleur,6).

% --------------------------------------------------------------- %

regle_rep(commence,1,
 [ qui, commence, le, jeu ],
 [ "par convention, c'est au joueur en charge des lutins verts de commencer la partie." ] ).

% ----------------------------------------------------------------% 

regle_rep(equipe,5,
  [ [ combien ], 3, [ lutins ], 5, [ equipe ] ],
  [ chaque, equipe, compte, X, lutins ]) :- 

       nb_lutins(X).

regle_rep(quipe,5,
  [ [ combien ], 3, [ lutin ], 5, [ quipe ] ],
  [ "chaque equipe compte ", X_in_chars, "lutins" ]) :- 

       nb_lutins(X),
       write_to_chars(X,X_in_chars).

write_to_chars(4,"4 ").

% ----------------------------------------------------------------%
% Nouvelles règles pour le bot

regle_rep(deplacer,1,
 [ [ comment ], 3, [ deplacer ], 3, [ lutin ] ],
 [ "pour deplacer un lutin, vous devez le faire passer d'une case a une autre en empruntant un pont.", "deux lutins ne peuvent pas se trouver sur la meme case." ] ).

regle_rep(deplac,1,
 [ [ puis ], 3, [ deplacer ], 3, [ lutin ], 3, [ occupe ] ],
 [ "non, vous ne pouvez pas deplacer un lutin sur une case occupee par un autre lutin." ] ).

regle_rep(retirer,1,
 [ [ quel ], 3, [ pont ], 3, [ retirer ] ],
 [ "il est permis de retirer le pont emprunte ou tout autre pont.", "vous pouvez aussi choisir de tourner un pont d'un quart de tour au lieu de le retirer." ] ).

regle_rep(pont,1,
 [ [ comment ], 3, [ retirer ], 3, [ pont ] ],
 [ "apres avoir deplace un lutin, vous devez retirer un pont ou tourner un pont d'un quart de tour.", "cela fait partie de votre tour de jeu." ] ).

regle_rep(regle,1,
 [ [ quelles ], 3, [ regles ], 3, [ jeu ] ],
 [ "pontu est un jeu ou vous deplacez vos lutins et retirez des ponts pour isoler les lutins adverses.", "a votre tour, vous deplacez un lutin en empruntant un pont, puis vous retirez ou tournez un pont.", "un joueur est elimine quand tous ses lutins sont isoles sans pont autour d'eux.", "le dernier joueur non elimine gagne la partie." ] ).

regle_rep(gagner,1,
 [ [ comment ], 3, [ gagner ] ],
 [ "le dernier joueur non elimine gagne la partie.", "pour cela, vous devez isoler les lutins de vos adversaires en retirant strategiquement les ponts." ] ).

regle_rep(plateau,1,
 [ [ taille ], 3, [ plateau ] ],
 [ "dans pontuXL, le plateau est de taille 6x6, contrairement a la version originale qui est en 5x5." ] ).

regle_rep(ordre,1,
 [ [ ordre ], 3, [ joueurs ] ],
 [ "les joueurs jouent dans l'ordre suivant : vert, bleu, jaune, rouge.", "contrairement a la version originale du jeu, les lutins bleus et rouges sont controles par une intelligence artificielle." ] ).

regle_rep(lutin,1,
 [ [ combien ], 3, [ lutins ], 3, [ joueur ] ],
 [ "chaque joueur dispose de 4 lutins de sa couleur." ] ).

regle_rep(fin,1,
 [ [ quand ], 3, [ fin ], 3, [ partie ] ],
 [ "la partie se termine lorsqu'il ne reste plus qu'un seul joueur dont les lutins ne sont pas tous isoles.", "ce joueur est declare vainqueur." ] ).

regle_rep(bonjour,1,
 [ bonjour ],
 [ "bonjour ! je suis pbot, le bot explicateur du jeu pontuXL.", "en quoi puis-je vous aider ?" ] ).

regle_rep(merci,1,
 [ merci ],
 [ "je vous en prie ! n'hesitez pas si vous avez d'autres questions." ] ).

regle_rep(ia,1,
 [ [ intelligence ], 3, [ artificielle ] ],
 [ "dans cette version du jeu, les lutins bleus et rouges sont controles par une intelligence artificielle basee sur des algorithmes de recherche de chemin.", "les lutins verts et jaunes sont controles par des joueurs humains." ] ).

regle_rep(jouer,1,
 [ [ comment ], 3, [ jouer ] ],
 [ "a votre tour, vous devez d'abord deplacer un de vos lutins d'une case a une autre en empruntant un pont.", "ensuite, vous devez retirer un pont (celui que vous avez utilise ou un autre) ou tourner un pont d'un quart de tour.", "le but est d'isoler les lutins adverses en retirant strategiquement les ponts." ] ).

regle_rep(elimine,1,
 [ [ quand ], 3, [ joueur ], 3, [ elimine ] ],
 [ "un joueur est elimine lorsque tous ses lutins se retrouvent sans pont autour d'eux, c'est-a-dire qu'ils sont isoles et ne peuvent plus se deplacer." ] ).

regle_rep(couleur,1,
 [ [ quelles ], 3, [ couleurs ], 3, [ lutins ] ],
 [ "dans pontuXL, il y a quatre couleurs de lutins : vert, bleu, rouge et jaune.", "les lutins verts et jaunes sont controles par des joueurs humains, tandis que les lutins bleus et rouges sont controles par l'intelligence artificielle." ] ).

% ----------------------------------------------------------------%

/*****************************************************************************/
% my_char_type(+Char,?Type)
%    Char is an ASCII code.
%    Type is whitespace, punctuation, numeric, alphabetic, or special.

my_char_type(46,period) :- !.
my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
my_char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
my_char_type(X,whitespace) :- X =< 32, !.
my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
my_char_type(X,punctuation) :- X >= 58, X =< 64, !.
my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
my_char_type(_,special).


/*****************************************************************************/
% lower_case(+C,?L)
%   If ASCII code C is an upper-case letter, then L is the
%   corresponding lower-case letter. Otherwise L=C.

lower_case(X,Y) :-
    X >= 65,
    X =< 90,
    Y is X + 32, !.

lower_case(X,X).


/*****************************************************************************/
% read_lc_string(-String)
%  Reads a line of input into String as a list of ASCII codes,
%  with all capital letters changed to lower case.

read_lc_string(String) :-
    get0(FirstChar),
    lower_case(FirstChar,LChar),
    read_lc_string_aux(LChar,String).

    read_lc_string_aux(10,[]) :- !.  % end of line

read_lc_string_aux(-1,[]) :- !.  % end of file

read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).


/*****************************************************************************/
% extract_word(+String,-Rest,-Word) (final version)
%  Extracts the first Word from String; Rest is rest of String.
%  A word is a series of contiguous letters, or a series
%  of contiguous digits, or a single special character.
%  Assumes String does not begin with whitespace.

extract_word([C|Chars],Rest,[C|RestOfWord]) :-
    my_char_type(C,Type),
    extract_word_aux(Type,Chars,Rest,RestOfWord).

    extract_word_aux(special,Rest,Rest,[]) :- !.
% if Char is special, don't read more chars.

extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
    my_char_type(C,Type), !,
extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.


/*****************************************************************************/
% remove_initial_blanks(+X,?Y)
%   Removes whitespace characters from the
%   beginning of string X, giving string Y.

remove_initial_blanks([C|Chars],Result) :-
    my_char_type(C,whitespace), !,
remove_initial_blanks(Chars,Result).

remove_initial_blanks(X,X).   % if previous clause did not succeed.


/*****************************************************************************/
% digit_value(?D,?V)
%  Where D is the ASCII code of a digit,
%  V is the corresponding number.

digit_value(48,0).
digit_value(49,1).
digit_value(50,2).
digit_value(51,3).
digit_value(52,4).
digit_value(53,5).
digit_value(54,6).
digit_value(55,7).
digit_value(56,8).
digit_value(57,9).


/*****************************************************************************/
% string_to_number(+S,-N)
%  Converts string S to the number that it
%  represents, e.g., "234" to 234.
%  Fails if S does not represent a nonnegative integer.

string_to_number(S,N) :-
    string_to_number_aux(S,0,N).

    string_to_number_aux([D|Digits],ValueSoFar,Result) :-
    digit_value(D,V),
    NewValueSoFar is 10*ValueSoFar + V,
string_to_number_aux(Digits,NewValueSoFar,Result).

string_to_number_aux([],Result,Result).


/*****************************************************************************/
% string_to_atomic(+String,-Atomic)
%  Converts String into the atom or number of
%  which it is the written representation.

string_to_atomic([C|Chars],Number) :-
    string_to_number([C|Chars],Number), !.

string_to_atomic(String,Atom) :- atom_codes(Atom,String).
% assuming previous clause failed.


/*****************************************************************************/
% extract_atomics(+String,-ListOfAtomics) (second version)
%  Breaks String up into ListOfAtomics
%  e.g., " abc def  123 " into [abc,def,123].

extract_atomics(String,ListOfAtomics) :-
    remove_initial_blanks(String,NewString),
    extract_atomics_aux(NewString,ListOfAtomics).

    extract_atomics_aux([C|Chars],[A|Atomics]) :-
    extract_word([C|Chars],Rest,Word),
    string_to_atomic(Word,A),       % <- this is the only change
extract_atomics(Rest,Atomics).

extract_atomics_aux([],[]).


/*****************************************************************************/
% clean_string(+String,-Cleanstring)
%  removes all punctuation characters from String and return Cleanstring

clean_string([C|Chars],L) :-
    my_char_type(C,punctuation),
    clean_string(Chars,L), !.
clean_string([C|Chars],[C|L]) :-
    clean_string(Chars,L), !.
clean_string([C|[]],[]) :-
    my_char_type(C,punctuation), !.
clean_string([C|[]],[C]).


/*****************************************************************************/
% read_atomics(-ListOfAtomics)
%  Reads a line of input, removes all punctuation characters, and converts
%  it into a list of atomic terms, e.g., [this,is,an,example].

read_atomics(ListOfAtomics) :-
    read_lc_string(String),
    clean_string(String,Cleanstring),
    extract_atomics(Cleanstring,ListOfAtomics).



/* --------------------------------------------------------------------- */
/*                                                                       */
/*        PRODUIRE_REPONSE : ecrit la liste de strings                   */
/*                                                                       */
/* --------------------------------------------------------------------- */

transformer_reponse_en_string(Li,Lo) :- flatten_strings_in_sentences(Li,Lo).

flatten_strings_in_sentences([],[]).
flatten_strings_in_sentences([W|T],S) :-
    string_as_list(W,L1),
    flatten_strings_in_sentences(T,L2),
    append(L1,L2,S).

% Pour SWI-Prolog
string_as_list(W,L) :- string_to_list(W,L).


% Pour tau-Prolog
% string_as_list(W,W).


/*    /!\ ci-après différent du code javascript
*/

/* --------------------------------------------------------------------- */
/*                                                                       */
/*        ECRIRE_REPONSE : ecrit une suite de lignes de texte            */
/*                                                                       */
/* --------------------------------------------------------------------- */


ecrire_reponse(L) :-
   nl, write('PBot :'),
   ecrire_ligne(L,1,1,Mf).

% ecrire_ligne(Li,Mi,Ei,Mf)
% input : Li, liste de mots a ecrire
%         Mi, indique si le premier caractere du premier mot 
%            doit etre mis en majuscule (1 si oui, 0 si non)
%         Ei, indique le nombre d'espaces avant ce premier mot 
% output : Mf, booleen tel que decrit ci-dessus a appliquer 
%          a la ligne suivante, si elle existe

ecrire_ligne([],M,_,M) :- 
   nl.

ecrire_ligne([M|L],Mi,Ei,Mf) :-
   ecrire_mot(M,Mi,Maux,Ei,Eaux),
   ecrire_ligne(L,Maux,Eaux,Mf).

% ecrire_mot(M,B1,B2,E1,E2)
% input : M, le mot a ecrire
%         B1, indique s'il faut une majuscule (1 si oui, 0 si non)
%         E1, indique s'il faut un espace avant le mot (1 si oui, 0 si non)
% output : B2, indique si le mot suivant prend une majuscule
%          E2, indique si le mot suivant doit etre precede d'un espace

ecrire_mot('.',_,1,_,1) :-
   write('. '), !.
ecrire_mot('\'',X,X,_,0) :-
   write('\''), !.
ecrire_mot(',',X,X,E,1) :-
   espace(E), write(','), !.
ecrire_mot(M,0,0,E,1) :-
   espace(E), write(M).
ecrire_mot(M,1,0,E,1) :-
   name(M,[C|L]),
   D is C - 32,
   name(N,[D|L]),
   espace(E), write(N).

espace(0).
espace(N) :- N>0, Nn is N-1, write(' '), espace(Nn).





/* --------------------------------------------------------------------- */
/*                                                                       */
/*                            TEST DE FIN                                */
/*                                                                       */
/* --------------------------------------------------------------------- */

fin(L) :- member(fin,L).


/* --------------------------------------------------------------------- */
/*                                                                       */
/*                         BOUCLE PRINCIPALE                             */
/*                                                                       */
/* --------------------------------------------------------------------- */

pontuXL :- 

   nl, nl, nl,
   write('Bonjour, je suis PBot, le bot explicateur du jeu PontuXL.'), nl,
   write('En quoi puis-je vous etre utile ?'), 
   nl, nl, 

   repeat,
      write('Vous : '), ttyflush,
      lire_question(L_Mots),
      produire_reponse(L_Mots,L_reponse),
      ecrire_reponse(L_reponse), nl,
   fin(L_Mots), !.
   

/* --------------------------------------------------------------------- */
/*                                                                       */
/*             ACTIVATION DU PROGRAMME APRES COMPILATION                 */
/*                                                                       */
/* --------------------------------------------------------------------- */

:- pontuXL.
