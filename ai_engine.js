/**
 * ai_engine.js
 * 
 * Implémentation d'un moteur d'IA multijoueur basé sur l'algorithme Maxⁿ avec élagage superficiel
 * selon l'article "Multi-player alpha-beta pruning" de Richard E. Korf.
 * 
 * Ce moteur d'IA est conçu pour des jeux multijoueurs à information parfaite comme PontuXL.
 */

// Constantes
const MAX_DEPTH = 3;           // Profondeur maximale de recherche
const MAX_EVALUATION = 100;    // Valeur maximale d'évaluation (somme des évaluations des joueurs)
const PLAYERS = ['green', 'blue', 'yellow', 'red']; // Ordre des joueurs

/**
 * Classe représentant l'état du jeu pour l'algorithme Maxⁿ
 */
class GameState {
    constructor(board, bridges, lutins, currentPlayer, activePlayers) {
        this.board = JSON.parse(JSON.stringify(board));           // Copie profonde du plateau
        this.bridges = JSON.parse(JSON.stringify(bridges));       // Copie profonde des ponts
        this.lutins = JSON.parse(JSON.stringify(lutins));         // Copie profonde des lutins
        this.currentPlayer = currentPlayer;                       // Joueur actuel
        this.activePlayers = [...activePlayers];                  // Joueurs actifs
    }

    /**
     * Génère tous les mouvements possibles pour le joueur actuel
     * @returns {Array} Liste des mouvements possibles sous forme [lutinIdx, targetRow, targetCol]
     */
    generateMoves() {
        const moves = [];
        const playerLutins = this.lutins[PLAYERS[this.currentPlayer]];
        
        // Pour chaque lutin du joueur actuel
        for (let lutinIdx = 0; lutinIdx < playerLutins.length; lutinIdx++) {
            const lutin = playerLutins[lutinIdx];
            
            // Vérifier les 4 directions (haut, bas, gauche, droite)
            const directions = [
                { row: -1, col: 0 }, // Haut
                { row: 1, col: 0 },  // Bas
                { row: 0, col: -1 }, // Gauche
                { row: 0, col: 1 }   // Droite
            ];
            
            for (const dir of directions) {
                const targetRow = lutin.row + dir.row;
                const targetCol = lutin.col + dir.col;
                
                // Vérifier si le mouvement est valide
                if (this.isValidMove(lutin.row, lutin.col, targetRow, targetCol)) {
                    moves.push([lutinIdx, targetRow, targetCol]);
                }
            }
        }
        
        return moves;
    }
    
    /**
     * Vérifie si un mouvement est valide
     * @param {number} fromRow - Ligne de départ
     * @param {number} fromCol - Colonne de départ
     * @param {number} toRow - Ligne d'arrivée
     * @param {number} toCol - Colonne d'arrivée
     * @returns {boolean} True si le mouvement est valide
     */
    isValidMove(fromRow, fromCol, toRow, toCol) {
        const BOARD_SIZE = this.board.length;
        
        // Vérifier si la destination est dans les limites du plateau
        if (toRow < 0 || toRow >= BOARD_SIZE || toCol < 0 || toCol >= BOARD_SIZE) {
            return false;
        }
        
        // Vérifier si la destination est vide
        if (this.board[toRow][toCol] !== null) {
            return false;
        }
        
        // Vérifier si le mouvement est adjacent
        const rowDiff = Math.abs(fromRow - toRow);
        const colDiff = Math.abs(fromCol - toCol);
        
        if ((rowDiff === 1 && colDiff === 0) || (rowDiff === 0 && colDiff === 1)) {
            // Vérifier s'il y a un pont entre les cellules
            if (rowDiff === 1) {
                // Mouvement vertical (vérifier pont horizontal)
                const bridgeRow = Math.min(fromRow, toRow);
                return this.bridges.horizontal[bridgeRow][fromCol];
            } else {
                // Mouvement horizontal (vérifier pont vertical)
                const bridgeCol = Math.min(fromCol, toCol);
                return this.bridges.vertical[fromRow][bridgeCol];
            }
        }
        
        return false;
    }
    
    /**
     * Génère tous les ponts qui peuvent être retirés
     * @returns {Array} Liste des ponts qui peuvent être retirés sous forme [type, row, col]
     */
    generateBridgesToRemove() {
        const bridges = [];
        const BOARD_SIZE = this.board.length;
        
        // Parcourir tous les ponts horizontaux
        for (let row = 0; row < BOARD_SIZE; row++) {
            for (let col = 0; col < BOARD_SIZE; col++) {
                if (this.bridges.horizontal[row][col]) {
                    bridges.push(['horizontal', row, col]);
                }
            }
        }
        
        // Parcourir tous les ponts verticaux
        for (let row = 0; row < BOARD_SIZE; row++) {
            for (let col = 0; col < BOARD_SIZE; col++) {
                if (this.bridges.vertical[row][col]) {
                    bridges.push(['vertical', row, col]);
                }
            }
        }
        
        return bridges;
    }
    
    /**
     * Applique un mouvement à l'état du jeu
     * @param {Array} move - Mouvement sous forme [lutinIdx, targetRow, targetCol]
     * @returns {GameState} Nouvel état du jeu après le mouvement
     */
    applyMove(move) {
        const [lutinIdx, targetRow, targetCol] = move;
        const newState = new GameState(this.board, this.bridges, this.lutins, this.currentPlayer, this.activePlayers);
        const currentPlayer = PLAYERS[this.currentPlayer];
        const lutin = newState.lutins[currentPlayer][lutinIdx];
        
        // Mettre à jour le plateau
        newState.board[lutin.row][lutin.col] = null;
        newState.board[targetRow][targetCol] = currentPlayer;
        
        // Mettre à jour la position du lutin
        newState.lutins[currentPlayer][lutinIdx] = { row: targetRow, col: targetCol };
        
        return newState;
    }
    
    /**
     * Applique une suppression de pont à l'état du jeu
     * @param {Array} bridge - Pont à supprimer sous forme [type, row, col]
     * @returns {GameState} Nouvel état du jeu après la suppression du pont
     */
    applyBridgeRemoval(bridge) {
        const [type, row, col] = bridge;
        const newState = new GameState(this.board, this.bridges, this.lutins, this.currentPlayer, this.activePlayers);
        
        // Supprimer le pont
        if (type === 'horizontal') {
            newState.bridges.horizontal[row][col] = false;
        } else {
            newState.bridges.vertical[row][col] = false;
        }
        
        // Vérifier si des joueurs sont éliminés
        newState.checkPlayerElimination();
        
        // Passer au joueur suivant
        newState.currentPlayer = (newState.currentPlayer + 1) % PLAYERS.length;
        
        // Si le joueur suivant n'est pas actif, passer au suivant
        while (!newState.activePlayers.includes(PLAYERS[newState.currentPlayer])) {
            newState.currentPlayer = (newState.currentPlayer + 1) % PLAYERS.length;
        }
        
        return newState;
    }
    
    /**
     * Vérifie si des joueurs sont éliminés
     */
    checkPlayerElimination() {
        for (const player of PLAYERS) {
            if (this.activePlayers.includes(player)) {
                const isEliminated = this.lutins[player].every(lutin => {
                    return !this.hasAvailableMoves(lutin.row, lutin.col);
                });
                
                if (isEliminated) {
                    // Supprimer le joueur des joueurs actifs
                    this.activePlayers = this.activePlayers.filter(p => p !== player);
                }
            }
        }
    }
    
    /**
     * Vérifie si un lutin a des mouvements disponibles
     * @param {number} row - Ligne du lutin
     * @param {number} col - Colonne du lutin
     * @returns {boolean} True si le lutin a des mouvements disponibles
     */
    hasAvailableMoves(row, col) {
        const BOARD_SIZE = this.board.length;
        
        // Vérifier les 4 directions (haut, bas, gauche, droite)
        const directions = [
            { row: -1, col: 0 }, // Haut
            { row: 1, col: 0 },  // Bas
            { row: 0, col: -1 }, // Gauche
            { row: 0, col: 1 }   // Droite
        ];
        
        for (const dir of directions) {
            const targetRow = row + dir.row;
            const targetCol = col + dir.col;
            
            // Vérifier si le mouvement est valide
            if (this.isValidMove(row, col, targetRow, targetCol)) {
                return true;
            }
        }
        
        return false;
    }
}

/**
 * Fonction d'évaluation pour l'algorithme Maxⁿ
 * Retourne un n-uplet d'évaluations pour chaque joueur
 * @param {GameState} state - État du jeu à évaluer
 * @returns {Array} Tableau des évaluations pour chaque joueur
 */
function evaluateState(state) {
    const evaluations = Array(PLAYERS.length).fill(0);
    const BOARD_SIZE = state.board.length;
    
    // Évaluer chaque joueur
    for (let playerIdx = 0; playerIdx < PLAYERS.length; playerIdx++) {
        const player = PLAYERS[playerIdx];
        
        // Si le joueur est éliminé, son évaluation est 0
        if (!state.activePlayers.includes(player)) {
            evaluations[playerIdx] = 0;
            continue;
        }
        
        let score = 0;
        const playerLutins = state.lutins[player];
        
        // 1. Mobilité : nombre de mouvements possibles
        let mobility = 0;
        for (const lutin of playerLutins) {
            // Vérifier les 4 directions
            const directions = [
                { row: -1, col: 0 }, // Haut
                { row: 1, col: 0 },  // Bas
                { row: 0, col: -1 }, // Gauche
                { row: 0, col: 1 }   // Droite
            ];
            
            for (const dir of directions) {
                const targetRow = lutin.row + dir.row;
                const targetCol = lutin.col + dir.col;
                
                if (state.isValidMove(lutin.row, lutin.col, targetRow, targetCol)) {
                    mobility++;
                }
            }
        }
        
        // 2. Contrôle du centre : bonus pour les lutins proches du centre
        let centerControl = 0;
        const center = (BOARD_SIZE - 1) / 2;
        for (const lutin of playerLutins) {
            const distanceToCenter = Math.abs(lutin.row - center) + Math.abs(lutin.col - center);
            centerControl += (BOARD_SIZE - distanceToCenter);
        }
        
        // 3. Cohésion : bonus si les lutins sont proches les uns des autres
        let cohesion = 0;
        for (let i = 0; i < playerLutins.length; i++) {
            for (let j = i + 1; j < playerLutins.length; j++) {
                const distance = Math.abs(playerLutins[i].row - playerLutins[j].row) + 
                                Math.abs(playerLutins[i].col - playerLutins[j].col);
                cohesion += (BOARD_SIZE * 2 - distance);
            }
        }
        
        // Combiner les facteurs avec des poids
        score = 0.5 * mobility + 0.3 * centerControl + 0.2 * cohesion;
        
        // Normaliser le score entre 0 et MAX_EVALUATION / PLAYERS.length
        const maxPossibleScore = 4 * 4 + 0.3 * 4 * BOARD_SIZE + 0.2 * 6 * BOARD_SIZE * 2;
        score = (score / maxPossibleScore) * (MAX_EVALUATION / PLAYERS.length);
        
        evaluations[playerIdx] = score;
    }
    
    // S'assurer que la somme des évaluations est <= MAX_EVALUATION
    const sum = evaluations.reduce((a, b) => a + b, 0);
    if (sum > MAX_EVALUATION) {
        const factor = MAX_EVALUATION / sum;
        for (let i = 0; i < evaluations.length; i++) {
            evaluations[i] *= factor;
        }
    }
    
    return evaluations;
}

/**
 * Algorithme Maxⁿ avec élagage superficiel (Shallow Pruning)
 * @param {GameState} state - État du jeu
 * @param {number} depth - Profondeur de recherche restante
 * @param {number} bound - Borne supérieure pour le joueur actuel
 * @returns {Array} Meilleure évaluation pour chaque joueur
 */
function shallowMaxN(state, depth, bound) {
    // Si on atteint un nœud terminal ou la profondeur maximale
    if (depth === 0 || state.activePlayers.length === 1) {
        return evaluateState(state);
    }
    
    const currentPlayer = state.currentPlayer;
    
    // Générer tous les mouvements possibles
    const moves = state.generateMoves();
    
    // Si aucun mouvement n'est possible, passer au joueur suivant
    if (moves.length === 0) {
        // Créer un nouvel état où le joueur actuel est passé
        const newState = new GameState(state.board, state.bridges, state.lutins, state.currentPlayer, state.activePlayers);
        newState.currentPlayer = (newState.currentPlayer + 1) % PLAYERS.length;
        
        // Si le joueur suivant n'est pas actif, passer au suivant
        while (!newState.activePlayers.includes(PLAYERS[newState.currentPlayer])) {
            newState.currentPlayer = (newState.currentPlayer + 1) % PLAYERS.length;
        }
        
        return shallowMaxN(newState, depth, bound);
    }
    
    // Initialiser la meilleure évaluation
    let bestEvaluation = Array(PLAYERS.length).fill(0);
    
    // Pour chaque mouvement possible
    for (let i = 0; i < moves.length; i++) {
        const move = moves[i];
        
        // Appliquer le mouvement
        const newState = state.applyMove(move);
        
        // Pour chaque pont qui peut être retiré
        const bridges = newState.generateBridgesToRemove();
        
        for (let j = 0; j < bridges.length; j++) {
            const bridge = bridges[j];
            
            // Appliquer la suppression du pont
            const nextState = newState.applyBridgeRemoval(bridge);
            
            // Calculer l'évaluation récursivement
            const evaluation = shallowMaxN(nextState, depth - 1, MAX_EVALUATION - bestEvaluation[currentPlayer]);
            
            // Mettre à jour la meilleure évaluation si nécessaire
            if (evaluation[currentPlayer] > bestEvaluation[currentPlayer]) {
                bestEvaluation = evaluation;
                
                // Élagage superficiel (Shallow Pruning)
                if (bestEvaluation[currentPlayer] >= bound) {
                    return bestEvaluation;
                }
            }
        }
    }
    
    return bestEvaluation;
}

/**
 * Trouve le meilleur mouvement pour le joueur actuel
 * @param {Object} gameState - État actuel du jeu
 * @returns {Object} Meilleur mouvement sous forme {lutin, targetCell, bridge}
 */
function findBestMove(gameState) {
    // Convertir l'état du jeu au format utilisé par l'algorithme
    const state = convertGameState(gameState);
    
    // Générer tous les mouvements possibles
    const moves = state.generateMoves();
    
    // Si aucun mouvement n'est possible, retourner null
    if (moves.length === 0) {
        return null;
    }
    
    let bestMove = null;
    let bestBridge = null;
    let bestEvaluation = -Infinity;
    
    // Pour chaque mouvement possible
    for (let i = 0; i < moves.length; i++) {
        const move = moves[i];
        
        // Appliquer le mouvement
        const newState = state.applyMove(move);
        
        // Pour chaque pont qui peut être retiré
        const bridges = newState.generateBridgesToRemove();
        
        for (let j = 0; j < bridges.length; j++) {
            const bridge = bridges[j];
            
            // Appliquer la suppression du pont
            const nextState = newState.applyBridgeRemoval(bridge);
            
            // Calculer l'évaluation avec l'algorithme Maxⁿ
            const evaluation = shallowMaxN(nextState, MAX_DEPTH, MAX_EVALUATION);
            
            // Mettre à jour le meilleur mouvement si nécessaire
            if (evaluation[state.currentPlayer] > bestEvaluation) {
                bestEvaluation = evaluation[state.currentPlayer];
                bestMove = move;
                bestBridge = bridge;
            }
        }
    }
    
    // Convertir le mouvement au format utilisé par le jeu
    if (bestMove) {
        const [lutinIdx, targetRow, targetCol] = bestMove;
        const [bridgeType, bridgeRow, bridgeCol] = bestBridge;
        
        const currentPlayer = PLAYERS[state.currentPlayer];
        const lutin = state.lutins[currentPlayer][lutinIdx];
        
        return {
            lutin: { row: lutin.row, col: lutin.col },
            targetCell: { row: targetRow, col: targetCol },
            bridge: { type: bridgeType, row: bridgeRow, col: bridgeCol }
        };
    }
    
    return null;
}

/**
 * Convertit l'état du jeu au format utilisé par l'algorithme
 * @param {Object} gameState - État du jeu au format du jeu
 * @returns {GameState} État du jeu au format de l'algorithme
 */
function convertGameState(gameState) {
    // Créer un tableau 2D pour le plateau
    const BOARD_SIZE = 6; // Taille du plateau de PontuXL
    const board = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(null));
    
    // Remplir le plateau avec les lutins
    for (const player of PLAYERS) {
        for (const lutin of gameState.lutins[player]) {
            board[lutin.row][lutin.col] = player;
        }
    }
    
    // Trouver l'index du joueur actuel
    const currentPlayerIndex = PLAYERS.indexOf(PLAYERS[gameState.currentPlayer]);
    
    return new GameState(
        board,
        gameState.bridges,
        gameState.lutins,
        currentPlayerIndex,
        gameState.activePlayers
    );
}
