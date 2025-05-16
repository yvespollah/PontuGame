# Projet PontuXL

Ce projet est une implémentation du jeu de société Pontu, développé dans le cadre du cours INFO B317 - IA et programmation symbolique.

## Description du jeu

Pontu est un jeu de société dont le but est d'isoler les lutins de son adversaire en déplaçant ses propres lutins et en retirant des ponts. Le jeu se pratique sur un plateau de 5x5 cases, les cases étant reliées entre elles par des ponts. Chaque joueur dispose de lutins de couleur différente.

Les règles principales sont:
- Un joueur déplace un de ses lutins d'une case à une autre en empruntant un pont
- Puis, à son choix, il retire un pont - celui qu'il a utilisé ou un autre - ou tourne un pont d'un quart de tour
- Deux lutins ne peuvent pas se trouver sur une même case
- Un joueur est éliminé lorsque tous ses lutins se retrouvent sans pont autour d'eux

## Particularités du projet

- Le jeu sera joué par quatre joueurs, utilisant des lutins de couleur bleu, vert, rouge et jaune
- Les lutins de couleur bleu et rouge seront déplacés par une intelligence artificielle
- Les joueurs jouent tour à tour dans l'ordre: verts, bleus, jaunes puis rouges
- Le plateau de jeu est de taille 6 sur 6
- Les cases sont numérotées selon les abscisses et ordonnées en prenant le coin inférieur gauche comme origine

## Intelligence Artificielle

Le jeu comprend une IA avancée implémentée en Prolog, utilisant l'algorithme Maxⁿ avec élagage superficiel, spécialement conçu pour les jeux à plus de deux joueurs.

### Algorithme Maxⁿ

Contrairement à l'algorithme Minimax traditionnel qui est conçu pour les jeux à deux joueurs (un joueur maximise son score, l'autre le minimise), l'algorithme Maxⁿ est adapté aux jeux multi-joueurs. Chaque joueur tente de maximiser son propre score, sans nécessairement minimiser celui des autres.

L'algorithme Maxⁿ fonctionne comme suit :

1. À chaque état du jeu, on évalue un n-uplet de scores (un score pour chaque joueur)
2. Chaque joueur choisit le coup qui maximise son propre score
3. L'élagage superficiel (Shallow Pruning) est utilisé pour optimiser la recherche

### Implémentation en Prolog

L'implémentation Prolog de l'algorithme Maxⁿ se trouve dans le fichier `pontu_ai.pl`. Les principales fonctions sont :

- `trouver_meilleur_coup/2` : Point d'entrée pour trouver le meilleur coup
- `maxn_shallow/4` : Implémentation de l'algorithme Maxⁿ avec élagage superficiel
- `evaluer_etat_tous_joueurs/2` : Évalue l'état du jeu pour tous les joueurs
- `obtenir_coup_ia_maxn/5` : Interface pour obtenir le coup de l'IA

### Intégration JavaScript-Prolog

L'intégration entre JavaScript et Prolog est réalisée grâce à la bibliothèque Tau Prolog, qui permet d'exécuter du code Prolog directement dans le navigateur. L'interface est implémentée dans le fichier `prolog_interface.js`.

Le processus d'intégration fonctionne comme suit :

1. Le code Prolog est chargé dans une session Tau Prolog
2. L'état du jeu JavaScript est converti en format Prolog
3. Une requête Prolog est exécutée pour trouver le meilleur coup
4. Le résultat est converti en format JavaScript et utilisé par le jeu

## Composants du projet

1. Interface web - représentation graphique du jeu (`index.html`, `game.js`, `styles.css`)
2. Bot explicateur - répond aux questions sur le jeu (implémenté en Prolog dans `pbot-elm.pl`)
3. Modules d'intelligence artificielle - pour guider les robots :
   - Algorithme Maxⁿ avec élagage superficiel (implémenté en Prolog dans `pontu_ai.pl`)
   - Interface JavaScript-Prolog (`prolog_interface.js`)

## Comment jouer

1. Ouvrez `index.html` dans un navigateur web moderne
2. Le jeu se joue à tour de rôle entre les joueurs humains (vert et jaune) et les IA (bleu et rouge)
3. À chaque tour, un joueur doit :
   - Sélectionner un lutin à déplacer
   - Déplacer le lutin vers une case adjacente accessible par un pont
   - Retirer un pont du plateau
4. Un joueur est éliminé lorsque tous ses lutins sont isolés (sans possibilité de mouvement)
5. Le dernier joueur avec des lutins mobiles gagne la partie

## Exécution

Pour exécuter le jeu, vous pouvez simplement ouvrir le fichier `index.html` dans un navigateur, ou utiliser un serveur web local :

```bash
python3 -m http.server 8000
```

Puis accéder à `http://localhost:8000` dans votre navigateur.

## Vidéo illustrative
Une vidéo illustrant une partie est disponible sur YouTube à l'adresse: https://www.youtube.com/watch?v=HYPHIm9OCQc
