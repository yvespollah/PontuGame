:- use_module(library(lists)).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                 Intelligence Artificielle pour PontuXL                */
/*                                                                       */
/* Module d'IA pour guider les robots (joueurs bleu et rouge)            */
/* Basé sur les algorithmes de recherche de chemin                       */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Constantes du jeu
taille_plateau(6).
nb_lutins_par_joueur(4).
couleurs_joueurs([vert, bleu, jaune, rouge]).
couleurs_ia([bleu, rouge]).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                     Structure de données du jeu                       */
/*                                                                       */
/* --------------------------------------------------------------------- */

% État du jeu
% etat_jeu(Plateau, Ponts, JoueurCourant, JoueursActifs)
% - Plateau: matrice 6x6 représentant les positions des lutins
% - Ponts: structure contenant les ponts horizontaux et verticaux
% - JoueurCourant: joueur dont c'est le tour (vert, bleu, jaune, rouge)
% - JoueursActifs: liste des joueurs encore en jeu

% Structure des ponts
% ponts(PontsH, PontsV)
% - PontsH: matrice 5x6 représentant les ponts horizontaux
% - PontsV: matrice 6x5 représentant les ponts verticaux

% Position d'un lutin
% pos(X, Y)

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                  Fonctions d'évaluation de l'état                     */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Évaluer l'état du jeu pour un joueur donné
evaluer_etat(Etat, Joueur, Score) :-
    etat_jeu(Plateau, Ponts, _, _) = Etat,
    % Calculer le score basé sur plusieurs facteurs
    mobilite_lutins(Plateau, Ponts, Joueur, ScoreMobilite),
    isolation_adversaires(Plateau, Ponts, Joueur, ScoreIsolation),
    Score is ScoreMobilite + ScoreIsolation.

% Calculer la mobilité des lutins d'un joueur
mobilite_lutins(Plateau, Ponts, Joueur, Score) :-
    trouver_lutins(Plateau, Joueur, Lutins),
    calculer_mobilite_totale(Lutins, Plateau, Ponts, 0, Score).

calculer_mobilite_totale([], _, _, Score, Score).
calculer_mobilite_totale([Lutin|Reste], Plateau, Ponts, ScoreAcc, Score) :-
    mouvements_possibles(Lutin, Plateau, Ponts, Mouvements),
    length(Mouvements, NbMouvements),
    NouveauScore is ScoreAcc + NbMouvements,
    calculer_mobilite_totale(Reste, Plateau, Ponts, NouveauScore, Score).

% Calculer le score d'isolation des adversaires
isolation_adversaires(Plateau, Ponts, Joueur, Score) :-
    couleurs_joueurs(Couleurs),
    delete(Couleurs, Joueur, Adversaires),
    calculer_isolation_totale(Adversaires, Plateau, Ponts, 0, Score).

calculer_isolation_totale([], _, _, Score, Score).
calculer_isolation_totale([Adversaire|Reste], Plateau, Ponts, ScoreAcc, Score) :-
    trouver_lutins(Plateau, Adversaire, Lutins),
    calculer_isolation_lutins(Lutins, Plateau, Ponts, 0, ScoreAdversaire),
    % Plus le score est élevé, plus l'adversaire est isolé (ce qui est bon pour nous)
    NouveauScore is ScoreAcc + ScoreAdversaire,
    calculer_isolation_totale(Reste, Plateau, Ponts, NouveauScore, Score).

calculer_isolation_lutins([], _, _, Score, Score).
calculer_isolation_lutins([Lutin|Reste], Plateau, Ponts, ScoreAcc, Score) :-
    mouvements_possibles(Lutin, Plateau, Ponts, Mouvements),
    length(Mouvements, NbMouvements),
    % Moins de mouvements = plus isolé = meilleur score pour nous
    ScoreIsolation is 4 - NbMouvements, % 4 est le maximum de mouvements possibles
    NouveauScore is ScoreAcc + ScoreIsolation,
    calculer_isolation_lutins(Reste, Plateau, Ponts, NouveauScore, Score).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                  Fonctions d'analyse du plateau                       */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Trouver tous les lutins d'un joueur sur le plateau
trouver_lutins(Plateau, Joueur, Lutins) :-
    taille_plateau(Taille),
    findall(pos(X, Y), 
            (between(0, Taille-1, X), 
             between(0, Taille-1, Y), 
             acceder_plateau(Plateau, X, Y, Joueur)), 
            Lutins).

% Accéder à une case du plateau
acceder_plateau(Plateau, X, Y, Valeur) :-
    nth0(Y, Plateau, Ligne),
    nth0(X, Ligne, Valeur).

% Vérifier si une position est dans les limites du plateau
position_valide(X, Y) :-
    taille_plateau(Taille),
    X >= 0, X < Taille,
    Y >= 0, Y < Taille.

% Vérifier si une case est vide
case_vide(Plateau, X, Y) :-
    position_valide(X, Y),
    acceder_plateau(Plateau, X, Y, vide).

% Vérifier si un pont existe entre deux cases adjacentes
pont_existe(Ponts, X1, Y1, X2, Y2) :-
    % Vérifier que les cases sont adjacentes
    (
        (X1 =:= X2, abs(Y1 - Y2) =:= 1) -> % Mouvement vertical
            MinY is min(Y1, Y2),
            ponts(PontsH, _) = Ponts,
            acceder_plateau(PontsH, X1, MinY, true)
        ;
        (Y1 =:= Y2, abs(X1 - X2) =:= 1) -> % Mouvement horizontal
            MinX is min(X1, X2),
            ponts(_, PontsV) = Ponts,
            acceder_plateau(PontsV, MinX, Y1, true)
        ;
            false % Les cases ne sont pas adjacentes
    ).

% Trouver tous les mouvements possibles pour un lutin
mouvements_possibles(pos(X, Y), Plateau, Ponts, Mouvements) :-
    findall(pos(NX, NY),
            (
                % Définir les 4 directions possibles
                member(delta(DX, DY), [delta(0, 1), delta(1, 0), delta(0, -1), delta(-1, 0)]),
                NX is X + DX,
                NY is Y + DY,
                % Vérifier si le mouvement est valide
                position_valide(NX, NY),
                case_vide(Plateau, NX, NY),
                pont_existe(Ponts, X, Y, NX, NY)
            ),
            Mouvements).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                  Algorithme Minimax avec élagage                      */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Trouver le meilleur coup avec l'algorithme Minimax
meilleur_coup(Etat, Joueur, Profondeur, MeilleurCoup) :-
    minimax(Etat, Joueur, Profondeur, -10000, 10000, MeilleurCoup, _).

% Implémentation de l'algorithme Minimax avec élagage alpha-beta
minimax(Etat, Joueur, 0, _, _, _, Score) :-
    % Cas de base: évaluer l'état du jeu
    evaluer_etat(Etat, Joueur, Score).

minimax(Etat, Joueur, Profondeur, Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    Profondeur > 0,
    etat_jeu(_, _, JoueurCourant, _) = Etat,
    coups_possibles(Etat, JoueurCourant, Coups),
    
    % Si aucun coup n'est possible, évaluer l'état actuel
    (Coups = [] ->
        evaluer_etat(Etat, Joueur, MeilleurScore),
        MeilleurCoup = aucun_coup
    ;
        % Sinon, explorer les coups possibles
        NouvelleProf is Profondeur - 1,
        (JoueurCourant = Joueur ->
            % Maximiser pour le joueur actuel
            minimax_max(Coups, Etat, Joueur, NouvelleProf, Alpha, Beta, MeilleurCoup, MeilleurScore)
        ;
            % Minimiser pour les adversaires
            minimax_min(Coups, Etat, Joueur, NouvelleProf, Alpha, Beta, MeilleurCoup, MeilleurScore)
        )
    ).

% Maximiser le score
minimax_max([], _, _, _, Alpha, _, aucun_coup, Alpha).
minimax_max([Coup|Reste], Etat, Joueur, Profondeur, Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    appliquer_coup(Etat, Coup, NouvelEtat),
    minimax(NouvelEtat, Joueur, Profondeur, Alpha, Beta, _, Score),
    
    % Mettre à jour Alpha
    (Score > Alpha ->
        NouvelAlpha = Score,
        CoupCandidat = Coup
    ;
        NouvelAlpha = Alpha,
        CoupCandidat = MeilleurCoup
    ),
    
    % Élagage Beta
    (NouvelAlpha >= Beta ->
        MeilleurCoup = CoupCandidat,
        MeilleurScore = NouvelAlpha
    ;
        minimax_max(Reste, Etat, Joueur, Profondeur, NouvelAlpha, Beta, CoupTemp, ScoreTemp),
        MeilleurCoup = CoupTemp,
        MeilleurScore = ScoreTemp
    ).

% Minimiser le score
minimax_min([], _, _, _, _, Beta, aucun_coup, Beta).
minimax_min([Coup|Reste], Etat, Joueur, Profondeur, Alpha, Beta, MeilleurCoup, MeilleurScore) :-
    appliquer_coup(Etat, Coup, NouvelEtat),
    minimax(NouvelEtat, Joueur, Profondeur, Alpha, Beta, _, Score),
    
    % Mettre à jour Beta
    (Score < Beta ->
        NouvelBeta = Score,
        CoupCandidat = Coup
    ;
        NouvelBeta = Beta,
        CoupCandidat = MeilleurCoup
    ),
    
    % Élagage Alpha
    (Alpha >= NouvelBeta ->
        MeilleurCoup = CoupCandidat,
        MeilleurScore = NouvelBeta
    ;
        minimax_min(Reste, Etat, Joueur, Profondeur, Alpha, NouvelBeta, CoupTemp, ScoreTemp),
        MeilleurCoup = CoupTemp,
        MeilleurScore = ScoreTemp
    ).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                  Génération et application des coups                  */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Structure d'un coup
% coup(DeplacerLutin, RetirerPont)
% - DeplacerLutin: deplacement(pos(X1, Y1), pos(X2, Y2))
% - RetirerPont: retirer_pont(Type, X, Y)
%   où Type est 'horizontal' ou 'vertical'

% Générer tous les coups possibles pour un joueur
coups_possibles(Etat, Joueur, Coups) :-
    etat_jeu(Plateau, Ponts, Joueur, _) = Etat,
    % Trouver tous les lutins du joueur
    trouver_lutins(Plateau, Joueur, Lutins),
    % Générer tous les déplacements possibles
    findall(deplacement(Lutin, Destination),
            (
                member(Lutin, Lutins),
                mouvements_possibles(Lutin, Plateau, Ponts, Destinations),
                member(Destination, Destinations)
            ),
            Deplacements),
    
    % Générer tous les ponts qui peuvent être retirés
    ponts_retirables(Ponts, PontsRetirables),
    
    % Combiner déplacements et retraits de ponts
    findall(coup(Deplacement, RetirerPont),
            (
                member(Deplacement, Deplacements),
                member(RetirerPont, PontsRetirables)
            ),
            Coups).

% Trouver tous les ponts qui peuvent être retirés
ponts_retirables(Ponts, PontsRetirables) :-
    ponts(PontsH, PontsV) = Ponts,
    taille_plateau(Taille),
    
    % Ponts horizontaux
    findall(retirer_pont(horizontal, X, Y),
            (
                between(0, Taille-2, Y),
                between(0, Taille-1, X),
                acceder_plateau(PontsH, X, Y, true)
            ),
            PontsH_Retirables),
    
    % Ponts verticaux
    findall(retirer_pont(vertical, X, Y),
            (
                between(0, Taille-1, Y),
                between(0, Taille-2, X),
                acceder_plateau(PontsV, X, Y, true)
            ),
            PontsV_Retirables),
    
    % Combiner les deux types de ponts
    append(PontsH_Retirables, PontsV_Retirables, PontsRetirables).

% Appliquer un coup à l'état du jeu
appliquer_coup(Etat, Coup, NouvelEtat) :-
    etat_jeu(Plateau, Ponts, JoueurCourant, JoueursActifs) = Etat,
    coup(Deplacement, RetirerPont) = Coup,
    
    % Appliquer le déplacement
    deplacement(pos(X1, Y1), pos(X2, Y2)) = Deplacement,
    appliquer_deplacement(Plateau, JoueurCourant, X1, Y1, X2, Y2, NouveauPlateau),
    
    % Appliquer le retrait de pont
    retirer_pont(Type, X, Y) = RetirerPont,
    appliquer_retrait_pont(Ponts, Type, X, Y, NouveauxPonts),
    
    % Passer au joueur suivant
    joueur_suivant(JoueurCourant, JoueursActifs, NouveauJoueur),
    
    % Vérifier si des joueurs sont éliminés
    verifier_eliminations(NouveauPlateau, NouveauxPonts, JoueursActifs, NouveauxJoueursActifs),
    
    % Créer le nouvel état
    NouvelEtat = etat_jeu(NouveauPlateau, NouveauxPonts, NouveauJoueur, NouveauxJoueursActifs).

% Appliquer un déplacement de lutin
appliquer_deplacement(Plateau, Joueur, X1, Y1, X2, Y2, NouveauPlateau) :-
    % Créer une copie du plateau
    copy_term(Plateau, NouveauPlateau),
    
    % Mettre à jour les cases
    modifier_plateau(NouveauPlateau, X1, Y1, vide),
    modifier_plateau(NouveauPlateau, X2, Y2, Joueur).

% Modifier une valeur dans le plateau
modifier_plateau(Plateau, X, Y, Valeur) :-
    nth0(Y, Plateau, Ligne),
    nth0(X, Ligne, _, ResteLigne),
    nth0(X, NouvelleLigne, Valeur, ResteLigne),
    nth0(Y, Plateau, _, RestePlateau),
    nth0(Y, NouveauPlateau, NouvelleLigne, RestePlateau),
    Plateau = NouveauPlateau.

% Appliquer un retrait de pont
appliquer_retrait_pont(Ponts, Type, X, Y, NouveauxPonts) :-
    ponts(PontsH, PontsV) = Ponts,
    
    % Créer des copies des matrices de ponts
    copy_term(PontsH, NouveauxPontsH),
    copy_term(PontsV, NouveauxPontsV),
    
    % Mettre à jour le pont approprié
    (Type = horizontal ->
        modifier_plateau(NouveauxPontsH, X, Y, false),
        NouveauxPonts = ponts(NouveauxPontsH, NouveauxPontsV)
    ;
        modifier_plateau(NouveauxPontsV, X, Y, false),
        NouveauxPonts = ponts(NouveauxPontsH, NouveauxPontsV)
    ).

% Déterminer le joueur suivant
joueur_suivant(JoueurCourant, JoueursActifs, NouveauJoueur) :-
    couleurs_joueurs(Couleurs),
    nth0(IndexCourant, Couleurs, JoueurCourant),
    IndexSuivant is (IndexCourant + 1) mod 4,
    nth0(IndexSuivant, Couleurs, JoueurCandidat),
    
    % Vérifier si le joueur candidat est encore actif
    (member(JoueurCandidat, JoueursActifs) ->
        NouveauJoueur = JoueurCandidat
    ;
        % Sinon, chercher le prochain joueur actif
        joueur_suivant(JoueurCandidat, JoueursActifs, NouveauJoueur)
    ).

% Vérifier si des joueurs sont éliminés
verifier_eliminations(Plateau, Ponts, JoueursActifs, NouveauxJoueursActifs) :-
    findall(Joueur,
            (
                member(Joueur, JoueursActifs),
                \+ est_elimine(Plateau, Ponts, Joueur)
            ),
            NouveauxJoueursActifs).

% Vérifier si un joueur est éliminé
est_elimine(Plateau, Ponts, Joueur) :-
    trouver_lutins(Plateau, Joueur, Lutins),
    % Un joueur est éliminé si tous ses lutins sont isolés
    forall(member(Lutin, Lutins),
           (
               mouvements_possibles(Lutin, Plateau, Ponts, Mouvements),
               Mouvements = []
           )).

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                  Interface pour l'IA du jeu                           */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Déterminer le meilleur coup pour un joueur IA
jouer_ia(Etat, Joueur, MeilleurCoup) :-
    % Vérifier que le joueur est bien contrôlé par l'IA
    couleurs_ia(CouleursIA),
    member(Joueur, CouleursIA),
    
    % Déterminer la profondeur de recherche
    % (peut être ajustée en fonction de la performance)
    Profondeur = 3,
    
    % Trouver le meilleur coup avec l'algorithme Minimax
    meilleur_coup(Etat, Joueur, Profondeur, MeilleurCoup).

% Prédicat principal pour obtenir le coup de l'IA
obtenir_coup_ia(Plateau, Ponts, Joueur, Deplacement, RetirerPont) :-
    % Créer l'état du jeu
    couleurs_joueurs(Couleurs),
    Etat = etat_jeu(Plateau, Ponts, Joueur, Couleurs),
    
    % Obtenir le meilleur coup
    jouer_ia(Etat, Joueur, MeilleurCoup),
    
    % Extraire le déplacement et le retrait de pont
    coup(Deplacement, RetirerPont) = MeilleurCoup.

/* --------------------------------------------------------------------- */
/*                                                                       */
/*                  Prédicats d'initialisation                           */
/*                                                                       */
/* --------------------------------------------------------------------- */

% Initialiser un plateau vide
initialiser_plateau(Plateau) :-
    taille_plateau(Taille),
    length(Plateau, Taille),
    maplist(initialiser_ligne(Taille), Plateau).

initialiser_ligne(Taille, Ligne) :-
    length(Ligne, Taille),
    maplist(=(vide), Ligne).

% Initialiser les ponts (tous présents au début)
initialiser_ponts(Ponts) :-
    taille_plateau(Taille),
    TailleH is Taille - 1,
    
    % Initialiser les ponts horizontaux
    length(PontsH, Taille),
    maplist(initialiser_ligne_ponts(Taille), PontsH),
    
    % Initialiser les ponts verticaux
    length(PontsV, Taille),
    maplist(initialiser_ligne_ponts(TailleH), PontsV),
    
    Ponts = ponts(PontsH, PontsV).

initialiser_ligne_ponts(Taille, Ligne) :-
    length(Ligne, Taille),
    maplist(=(true), Ligne).

% Initialiser l'état du jeu
initialiser_etat(Etat) :-
    initialiser_plateau(Plateau),
    initialiser_ponts(Ponts),
    placer_lutins(Plateau, NouveauPlateau),
    couleurs_joueurs(Couleurs),
    Etat = etat_jeu(NouveauPlateau, Ponts, vert, Couleurs).

% Placer les lutins sur le plateau
placer_lutins(Plateau, NouveauPlateau) :-
    copy_term(Plateau, NouveauPlateau),
    taille_plateau(Taille),
    Last is Taille - 1,
    
    % Placer les lutins verts (coin supérieur gauche)
    modifier_plateau(NouveauPlateau, 0, 0, vert),
    modifier_plateau(NouveauPlateau, 1, 0, vert),
    modifier_plateau(NouveauPlateau, 0, 1, vert),
    modifier_plateau(NouveauPlateau, 1, 1, vert),
    
    % Placer les lutins bleus (coin supérieur droit)
    modifier_plateau(NouveauPlateau, Last-1, 0, bleu),
    modifier_plateau(NouveauPlateau, Last, 0, bleu),
    modifier_plateau(NouveauPlateau, Last-1, 1, bleu),
    modifier_plateau(NouveauPlateau, Last, 1, bleu),
    
    % Placer les lutins jaunes (coin inférieur gauche)
    modifier_plateau(NouveauPlateau, 0, Last-1, jaune),
    modifier_plateau(NouveauPlateau, 1, Last-1, jaune),
    modifier_plateau(NouveauPlateau, 0, Last, jaune),
    modifier_plateau(NouveauPlateau, 1, Last, jaune),
    
    % Placer les lutins rouges (coin inférieur droit)
    modifier_plateau(NouveauPlateau, Last-1, Last-1, rouge),
    modifier_plateau(NouveauPlateau, Last, Last-1, rouge),
    modifier_plateau(NouveauPlateau, Last-1, Last, rouge),
    modifier_plateau(NouveauPlateau, Last, Last, rouge).
