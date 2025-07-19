# PontuXL Game

PontuXL is a web-based implementation of the board game Pontu, enhanced with a modern user interface, smooth animations, and a powerful AI. This project was developed as part of the INFO B317 course on AI and Symbolic Programming.

## Game Overview

Pontu is a strategy game where the objective is to isolate your opponent's pieces ("lutins") by moving your own and strategically altering the game board. The last player with connected lutins wins.

The game is played on a 6x6 grid connected by bridges. Four players (Green, Blue, Yellow, Red) take turns to move their lutins and manipulate bridges.

-   **Human Players:** Green and Yellow
-   **AI Players:** Blue and Red

### Game Phases

1.  **Placement Phase:** Players take turns placing their two lutins on empty cells.
2.  **Main Phase:** Once all lutins are placed, players take turns in a cycle (Green -> Blue -> Yellow -> Red).

### A Player's Turn

A turn consists of two actions:

1.  **Move a Lutin:** Move one of your lutins across an adjacent bridge to an empty cell.
2.  **Action on a Bridge:** After moving, you must perform one of the following actions:
    *   **Remove a Bridge:** Click on any bridge on the board to remove it permanently.
    *   **Rotate a Bridge:** Click on any bridge to rotate it 90 degrees, changing its orientation (horizontal to vertical or vice-versa).

A player is eliminated when all of their lutins are isolated (i.e., have no bridges connected to their cells).

## Key Features

-   **Modern & Responsive UI:** A clean, intuitive interface that works on various screen sizes.
-   **Rich Animations:** Smooth animations for lutin movements, bridge removals, and rotations enhance the user experience.
-   **Interactive Game Board:** Cells and bridges highlight on hover, and selectable items are clearly indicated.
-   **Bridge Rotation Mechanic:** Adds a new strategic layer to the game, allowing players to reshape the board's connectivity.
-   **Action Modal:** A clear and accessible modal appears when a bridge is clicked, prompting the player to choose between removing or rotating it.
-   **AI Visual Feedback:** When the AI removes a bridge, it briefly pulses red to clearly indicate its move.
-   **Animated Victory Screen:** A polished, animated overlay announces the winner and allows for a quick rematch.

## How to Play

1.  **Open the Game:** Launch `index.html` in a modern web browser (e.g., Chrome, Firefox).
2.  **Placement Phase:** Click on any empty cell to place your lutins. The game will guide you through the player order.
3.  **Main Phase (Your Turn):**
    *   **Select a Lutin:** Click on one of your lutins. Valid destination cells will be highlighted.
    *   **Move:** Click on a highlighted cell to move your lutin.
    *   **Bridge Action:** After moving, click on any bridge. A modal will ask if you want to **Remove** or **Rotate** it. The AI will automatically perform its action.
4.  **Win Condition:** The game ends when only one player has connected lutins left.

## Artificial Intelligence

The AI players (Blue and Red) are powered by a sophisticated Prolog engine running directly in the browser via Tau Prolog.

### Maxⁿ Algorithm

Instead of the traditional Minimax algorithm for two-player games, PontuXL uses the **Maxⁿ algorithm**, which is designed for multi-player scenarios. In this model, each player aims to maximize their own score, assuming the other players will do the same. This approach is ideal for a 4-player free-for-all game.

The search is optimized using **Shallow Pruning** to reduce the search space and improve performance.

### Prolog Integration

The JavaScript front-end communicates with the Prolog engine (`pontu_ai.pl`) via an interface in `prolog_interface.js`.

1.  The current game state (lutin positions, bridge layout) is converted into Prolog terms.
2.  The Prolog engine executes the Maxⁿ algorithm to determine the best possible move for the AI player.
3.  The chosen move is returned to the JavaScript code, which then executes it on the game board with corresponding animations.
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
