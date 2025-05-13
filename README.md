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

## Composants du projet

1. Interface web - représentation graphique du jeu
2. Bot explicateur - répond aux questions sur le jeu (implémenté en Prolog)
3. Modules d'intelligence artificielle - pour guider les robots

## Vidéo illustrative
Une vidéo illustrant une partie est disponible sur YouTube à l'adresse: https://www.youtube.com/watch?v=HYPHIm9OCQc
