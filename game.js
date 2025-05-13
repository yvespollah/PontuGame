/**
 * PontuXL Game Logic
 * This file contains the main game logic for the PontuXL game
 */

// Game constants
const BOARD_SIZE = 6;
const PLAYERS = ['green', 'blue', 'yellow', 'red'];
const LUTINS_PER_PLAYER = 4;
const AI_PLAYERS = ['blue', 'red']; // Players controlled by AI
const HUMAN_PLAYERS = ['green', 'yellow']; // Players controlled by humans

// Game state
let gameState = {
    currentPlayer: 0, // Index in PLAYERS array
    selectedLutin: null,
    gamePhase: 'select', // 'select', 'move', 'remove'
    gameOver: false,
    activePlayers: [...PLAYERS], // Players still in the game
    board: [], // 2D array representing the board
    bridges: {
        horizontal: [], // 2D array representing horizontal bridges
        vertical: [] // 2D array representing vertical bridges
    },
    lutins: {} // Object mapping player color to array of lutin positions
};

// DOM elements
const gameBoard = document.getElementById('game-board');
const currentPlayerDisplay = document.getElementById('current-player');
const gameMessage = document.getElementById('game-message');
const resetButton = document.getElementById('reset-game');
const helpButton = document.getElementById('help-button');
const helpModal = document.getElementById('help-modal');
const closeModalButton = document.querySelector('.close');

// Initialize the game
function initGame() {
    // Create the board structure
    createBoard();
    
    // Initialize game state
    resetGameState();
    
    // Place lutins on the board
    placeLutins();
    
    // Render the initial board state
    renderBoard();
    
    // Update UI
    updateGameInfo();
    
    // Activer les lutins du premier joueur (vert)
    enableHumanPlayerLutins();
}

// Create the board structure
function createBoard() {
    gameBoard.innerHTML = '';
    
    // Create cells
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.dataset.row = row;
            cell.dataset.col = col;
            cell.addEventListener('click', handleCellClick);
            gameBoard.appendChild(cell);
            
            // Create horizontal bridges (top and bottom of each cell)
            if (row > 0) {
                const hBridge = document.createElement('div');
                hBridge.className = 'bridge bridge-horizontal top';
                hBridge.dataset.row = row;
                hBridge.dataset.col = col;
                hBridge.dataset.type = 'horizontal';
                hBridge.dataset.position = 'top';
                
                // Ajouter l'événement de clic directement
                hBridge.addEventListener('click', function(event) {
                    console.log("Clic sur pont horizontal top");
                    handleBridgeClick(event);
                });
                
                cell.appendChild(hBridge);
            }
            
            if (row < BOARD_SIZE - 1) {
                const hBridge = document.createElement('div');
                hBridge.className = 'bridge bridge-horizontal bottom';
                hBridge.dataset.row = row;
                hBridge.dataset.col = col;
                hBridge.dataset.type = 'horizontal';
                hBridge.dataset.position = 'bottom';
                
                // Ajouter l'événement de clic directement
                hBridge.addEventListener('click', function(event) {
                    console.log("Clic sur pont horizontal bottom");
                    handleBridgeClick(event);
                });
                
                cell.appendChild(hBridge);
            }
            
            // Create vertical bridges (left and right of each cell)
            if (col > 0) {
                const vBridge = document.createElement('div');
                vBridge.className = 'bridge bridge-vertical left';
                vBridge.dataset.row = row;
                vBridge.dataset.col = col;
                vBridge.dataset.type = 'vertical';
                vBridge.dataset.position = 'left';
                
                // Ajouter l'événement de clic directement
                vBridge.addEventListener('click', function(event) {
                    console.log("Clic sur pont vertical left");
                    handleBridgeClick(event);
                });
                
                cell.appendChild(vBridge);
            }
            
            if (col < BOARD_SIZE - 1) {
                const vBridge = document.createElement('div');
                vBridge.className = 'bridge bridge-vertical right';
                vBridge.dataset.row = row;
                vBridge.dataset.col = col;
                vBridge.dataset.type = 'vertical';
                vBridge.dataset.position = 'right';
                
                // Ajouter l'événement de clic directement
                vBridge.addEventListener('click', function(event) {
                    console.log("Clic sur pont vertical right");
                    handleBridgeClick(event);
                });
                
                cell.appendChild(vBridge);
            }
        }
    }
}

// Reset game state
function resetGameState() {
    // Initialize board
    gameState.board = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(null));
    
    // Initialize bridges
    gameState.bridges.horizontal = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(true));
    gameState.bridges.vertical = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(true));
    
    // Reset game variables
    gameState.currentPlayer = 0;
    gameState.selectedLutin = null;
    gameState.gamePhase = 'select';
    gameState.gameOver = false;
    gameState.activePlayers = [...PLAYERS];
    gameState.lutins = {};
    
    PLAYERS.forEach(player => {
        gameState.lutins[player] = [];
    });
}

// Place lutins on the board
function placeLutins() {
    // Green lutins (top-left)
    gameState.lutins.green = [
        {row: 0, col: 0},
        {row: 0, col: 1},
        {row: 1, col: 0},
        {row: 1, col: 1}
    ];
    
    // Blue lutins (top-right)
    gameState.lutins.blue = [
        {row: 0, col: BOARD_SIZE - 2},
        {row: 0, col: BOARD_SIZE - 1},
        {row: 1, col: BOARD_SIZE - 2},
        {row: 1, col: BOARD_SIZE - 1}
    ];
    
    // Yellow lutins (bottom-left)
    gameState.lutins.yellow = [
        {row: BOARD_SIZE - 2, col: 0},
        {row: BOARD_SIZE - 2, col: 1},
        {row: BOARD_SIZE - 1, col: 0},
        {row: BOARD_SIZE - 1, col: 1}
    ];
    
    // Red lutins (bottom-right)
    gameState.lutins.red = [
        {row: BOARD_SIZE - 2, col: BOARD_SIZE - 2},
        {row: BOARD_SIZE - 2, col: BOARD_SIZE - 1},
        {row: BOARD_SIZE - 1, col: BOARD_SIZE - 2},
        {row: BOARD_SIZE - 1, col: BOARD_SIZE - 1}
    ];
    
    // Update board with lutin positions
    Object.entries(gameState.lutins).forEach(([color, positions]) => {
        positions.forEach(pos => {
            gameState.board[pos.row][pos.col] = color;
        });
    });
}

// Render the board based on current game state
function renderBoard() {
    // Clear all lutins from the board
    document.querySelectorAll('.lutin').forEach(lutin => lutin.remove());
    
    // Render lutins
    Object.entries(gameState.lutins).forEach(([color, positions]) => {
        positions.forEach(pos => {
            const cell = document.querySelector(`.cell[data-row="${pos.row}"][data-col="${pos.col}"]`);
            if (cell) {
                const lutin = document.createElement('div');
                lutin.className = `lutin ${color}`;
                lutin.dataset.row = pos.row;
                lutin.dataset.col = pos.col;
                lutin.dataset.color = color;
                
                // Add click event for current player's lutins if they are human
                const currentPlayer = PLAYERS[gameState.currentPlayer];
                if (color === currentPlayer && HUMAN_PLAYERS.includes(currentPlayer)) {
                    lutin.addEventListener('click', handleLutinClick);
                    lutin.style.cursor = 'pointer'; // Changer le curseur pour indiquer que le lutin est cliquable
                }
                
                cell.appendChild(lutin);
            }
        });
    });
    
    // Update bridges visibility
    updateBridgesVisibility();
    
    // Highlight selected lutin if any
    if (gameState.selectedLutin) {
        const selectedLutinElement = document.querySelector(`.lutin[data-row="${gameState.selectedLutin.row}"][data-col="${gameState.selectedLutin.col}"]`);
        if (selectedLutinElement) {
            selectedLutinElement.classList.add('selected');
        }
    }
    
    // Debug: Afficher des informations sur le joueur actuel
    console.log(`Joueur actuel: ${PLAYERS[gameState.currentPlayer]}, Phase: ${gameState.gamePhase}`);
    console.log(`Est un joueur humain: ${HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])}`);
}

// Update bridges visibility based on game state
function updateBridgesVisibility() {
    // Update horizontal bridges
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            // Top bridges
            if (row > 0) {
                const bridge = document.querySelector(`.bridge-horizontal.top[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const isVisible = gameState.bridges.horizontal[row - 1][col];
                    bridge.style.display = isVisible ? 'block' : 'none';
                    
                    // Ajouter l'événement de clic si nous sommes en phase de suppression de pont
                    if (isVisible && gameState.gamePhase === 'remove' && HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                        
                        // Cloner pour supprimer les événements existants
                        const newBridge = bridge.cloneNode(true);
                        bridge.parentNode.replaceChild(newBridge, bridge);
                        
                        // Ajouter le nouvel événement de clic
                        newBridge.addEventListener('click', handleBridgeClick);
                    }
                }
            }
            
            // Bottom bridges
            if (row < BOARD_SIZE - 1) {
                const bridge = document.querySelector(`.bridge-horizontal.bottom[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const isVisible = gameState.bridges.horizontal[row][col];
                    bridge.style.display = isVisible ? 'block' : 'none';
                    
                    // Ajouter l'événement de clic si nous sommes en phase de suppression de pont
                    if (isVisible && gameState.gamePhase === 'remove' && HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                        
                        // Cloner pour supprimer les événements existants
                        const newBridge = bridge.cloneNode(true);
                        bridge.parentNode.replaceChild(newBridge, bridge);
                        
                        // Ajouter le nouvel événement de clic
                        newBridge.addEventListener('click', handleBridgeClick);
                    }
                }
            }
            
            // Left bridges
            if (col > 0) {
                const bridge = document.querySelector(`.bridge-vertical.left[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const isVisible = gameState.bridges.vertical[row][col - 1];
                    bridge.style.display = isVisible ? 'block' : 'none';
                    
                    // Ajouter l'événement de clic si nous sommes en phase de suppression de pont
                    if (isVisible && gameState.gamePhase === 'remove' && HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                        
                        // Cloner pour supprimer les événements existants
                        const newBridge = bridge.cloneNode(true);
                        bridge.parentNode.replaceChild(newBridge, bridge);
                        
                        // Ajouter le nouvel événement de clic
                        newBridge.addEventListener('click', handleBridgeClick);
                    }
                }
            }
            
            // Right bridges
            if (col < BOARD_SIZE - 1) {
                const bridge = document.querySelector(`.bridge-vertical.right[data-row="${row}"][data-col="${col}"]`);
                if (bridge) {
                    const isVisible = gameState.bridges.vertical[row][col];
                    bridge.style.display = isVisible ? 'block' : 'none';
                    
                    // Ajouter l'événement de clic si nous sommes en phase de suppression de pont
                    if (isVisible && gameState.gamePhase === 'remove' && HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
                        bridge.style.cursor = 'pointer';
                        
                        // Cloner pour supprimer les événements existants
                        const newBridge = bridge.cloneNode(true);
                        bridge.parentNode.replaceChild(newBridge, bridge);
                        
                        // Ajouter le nouvel événement de clic
                        newBridge.addEventListener('click', handleBridgeClick);
                    }
                }
            }
        }
    }
}

// Update game information display
function updateGameInfo() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    currentPlayerDisplay.textContent = currentPlayer.charAt(0).toUpperCase() + currentPlayer.slice(1);
    currentPlayerDisplay.style.color = getPlayerColor(currentPlayer);
    
    // Update message based on game phase
    switch (gameState.gamePhase) {
        case 'select':
            if (HUMAN_PLAYERS.includes(currentPlayer)) {
                gameMessage.textContent = 'Sélectionnez un lutin à déplacer.';
            } else {
                gameMessage.textContent = `L'IA réfléchit pour le joueur ${currentPlayer}...`;
            }
            break;
        case 'move':
            gameMessage.textContent = 'Déplacez le lutin vers une case adjacente.';
            break;
        case 'remove':
            gameMessage.textContent = 'Retirez un pont.';
            break;
    }
}

// Fonction qui gère le tour du joueur actuel
function handlePlayerTurn() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    // Si c'est un joueur IA et que le jeu n'est pas terminé
    if (AI_PLAYERS.includes(currentPlayer) && !gameState.gameOver) {
        // Jouer le tour de l'IA après 3 secondes
        setTimeout(playAITurn, 3000);
    } 
    // Si c'est un joueur humain
    else if (HUMAN_PLAYERS.includes(currentPlayer) && !gameState.gameOver) {
        // Activer les lutins du joueur humain pour qu'ils soient cliquables
        enableHumanPlayerLutins();
    }
}

// Fonction pour activer les lutins du joueur humain
function enableHumanPlayerLutins() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    // Désactiver tous les lutins d'abord
    document.querySelectorAll('.lutin').forEach(lutin => {
        lutin.style.cursor = 'default';
        
        // Cloner pour supprimer les événements
        const newLutin = lutin.cloneNode(true);
        lutin.parentNode.replaceChild(newLutin, lutin);
    });
    
    // Activer uniquement les lutins du joueur humain actuel
    document.querySelectorAll(`.lutin.${currentPlayer}`).forEach(lutin => {
        lutin.addEventListener('click', handleLutinClick);
        lutin.style.cursor = 'pointer';
    });
    
    console.log(`Lutins activés pour le joueur ${currentPlayer}`);
}

// Handle lutin click
function handleLutinClick(event) {
    if (gameState.gameOver) return;
    
    const lutin = event.target;
    const row = parseInt(lutin.dataset.row);
    const col = parseInt(lutin.dataset.col);
    const color = lutin.dataset.color;
    
    // Make sure it's the current player's lutin
    if (color !== PLAYERS[gameState.currentPlayer]) return;
    
    // If we're in the select phase, select the lutin
    if (gameState.gamePhase === 'select') {
        // Select the lutin
        gameState.selectedLutin = { row, col, color };
        gameState.gamePhase = 'move';
        
        // Highlight the selected lutin
        lutin.classList.add('selected');
        
        // Update game info
        updateGameInfo();
    }
}

// Handle cell click for moving a lutin
function handleCellClick(event) {
    if (gameState.gameOver) return;
    
    // S'assurer que nous cliquons sur une cellule et pas un autre élément
    const cell = event.target.closest('.cell');
    if (!cell) return;
    
    const row = parseInt(cell.dataset.row);
    const col = parseInt(cell.dataset.col);
    
    // Si nous sommes en phase de déplacement et qu'un lutin est sélectionné
    if (gameState.gamePhase === 'move' && gameState.selectedLutin) {
        const selectedLutin = gameState.selectedLutin;
        
        // Vérifier si le déplacement est valide
        if (isValidMove(selectedLutin.row, selectedLutin.col, row, col)) {
            // Déplacer le lutin
            moveLutin(selectedLutin.row, selectedLutin.col, row, col);
            
            // Passer à la phase de suppression de pont
            gameState.gamePhase = 'remove';
            
            // Mettre à jour l'interface
            renderBoard();
            updateGameInfo();
        }
    }
}

// Handle bridge click for removing a bridge
function handleBridgeClick(event) {
    if (gameState.gameOver) return;
    
    console.log("Clic sur un pont détecté!");
    
    // Obtenir l'élément pont directement
    const bridge = event.currentTarget;
    
    const row = parseInt(bridge.dataset.row);
    const col = parseInt(bridge.dataset.col);
    const type = bridge.dataset.type;
    const position = bridge.dataset.position;
    
    console.log(`Pont cliqué: type=${type}, position=${position}, row=${row}, col=${col}`);
    
    // Si nous sommes en phase de suppression de pont
    if (gameState.gamePhase === 'remove') {
        console.log("Phase de suppression de pont confirmée");
        
        // Déterminer les coordonnées exactes du pont à supprimer
        let bridgeRow = row;
        let bridgeCol = col;
        
        if (type === 'horizontal') {
            if (position === 'top') {
                bridgeRow = row - 1;
            }
        } else if (type === 'vertical') {
            if (position === 'left') {
                bridgeCol = col - 1;
            }
        }
        
        console.log(`Coordonnées du pont à supprimer: row=${bridgeRow}, col=${bridgeCol}`);
        
        // Supprimer le pont
        if (type === 'horizontal') {
            gameState.bridges.horizontal[bridgeRow][bridgeCol] = false;
            console.log(`Pont horizontal supprimé à [${bridgeRow}, ${bridgeCol}]`);
        } else {
            gameState.bridges.vertical[bridgeRow][bridgeCol] = false;
            console.log(`Pont vertical supprimé à [${bridgeRow}, ${bridgeCol}]`);
        }
        
        // Vérifier si un joueur est éliminé
        checkPlayerElimination();
        
        // Passer au joueur suivant
        gameState.gamePhase = 'select';
        gameState.selectedLutin = null;
        gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
        
        // Si le joueur suivant n'est pas actif, passer au suivant
        while (!gameState.activePlayers.includes(PLAYERS[gameState.currentPlayer])) {
            gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
        }
        
        // Mettre à jour l'interface
        renderBoard();
        updateGameInfo();
        
        // Vérifier si le joueur suivant est humain ou IA
        const nextPlayer = PLAYERS[gameState.currentPlayer];
        if (AI_PLAYERS.includes(nextPlayer) && !gameState.gameOver) {
            setTimeout(playAITurn, 3000);
        } else {
            enableHumanPlayerLutins();
        }
    } else {
        console.log(`Phase actuelle: ${gameState.gamePhase}, pas en phase de suppression de pont`);
    }
}

// Check if a move is valid
function isValidMove(fromRow, fromCol, toRow, toCol) {
    // Check if target cell is empty
    if (gameState.board[toRow][toCol] !== null) {
        return false;
    }
    
    // Check if the move is to an adjacent cell
    const rowDiff = Math.abs(fromRow - toRow);
    const colDiff = Math.abs(fromCol - toCol);
    
    if ((rowDiff === 1 && colDiff === 0) || (rowDiff === 0 && colDiff === 1)) {
        // Check if there's a bridge between the cells
        if (rowDiff === 1) {
            // Vertical movement (check horizontal bridge)
            const bridgeRow = Math.min(fromRow, toRow);
            return gameState.bridges.horizontal[bridgeRow][fromCol];
        } else {
            // Horizontal movement (check vertical bridge)
            const bridgeCol = Math.min(fromCol, toCol);
            return gameState.bridges.vertical[fromRow][bridgeCol];
        }
    }
    
    return false;
}

// Move a lutin from one cell to another
function moveLutin(fromRow, fromCol, toRow, toCol) {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    // Update board state
    gameState.board[fromRow][fromCol] = null;
    gameState.board[toRow][toCol] = currentPlayer;
    
    // Update lutin position in the player's lutins array
    const lutinIndex = gameState.lutins[currentPlayer].findIndex(
        lutin => lutin.row === fromRow && lutin.col === fromCol
    );
    
    if (lutinIndex !== -1) {
        gameState.lutins[currentPlayer][lutinIndex] = { row: toRow, col: toCol };
    }
    
    // Reset selected lutin
    gameState.selectedLutin = null;
}

// Check if any player is eliminated
function checkPlayerElimination() {
    PLAYERS.forEach(player => {
        if (gameState.activePlayers.includes(player)) {
            const isEliminated = gameState.lutins[player].every(lutin => {
                return !hasAvailableMoves(lutin.row, lutin.col);
            });
            
            if (isEliminated) {
                // Remove player from active players
                gameState.activePlayers = gameState.activePlayers.filter(p => p !== player);
                
                // Check if game is over
                if (gameState.activePlayers.length === 1) {
                    gameState.gameOver = true;
                    gameMessage.textContent = `Le joueur ${gameState.activePlayers[0].toUpperCase()} a gagné la partie!`;
                }
            }
        }
    });
}

// Check if a lutin has any available moves
function hasAvailableMoves(row, col) {
    // Check all adjacent cells
    const adjacentCells = [
        { row: row - 1, col: col }, // Up
        { row: row + 1, col: col }, // Down
        { row: row, col: col - 1 }, // Left
        { row: row, col: col + 1 }  // Right
    ];
    
    return adjacentCells.some(cell => {
        // Check if cell is within board bounds
        if (cell.row >= 0 && cell.row < BOARD_SIZE && cell.col >= 0 && cell.col < BOARD_SIZE) {
            // Check if cell is empty
            if (gameState.board[cell.row][cell.col] === null) {
                // Check if there's a bridge between the cells
                if (cell.row !== row) {
                    // Vertical movement (check horizontal bridge)
                    const bridgeRow = Math.min(row, cell.row);
                    return gameState.bridges.horizontal[bridgeRow][col];
                } else {
                    // Horizontal movement (check vertical bridge)
                    const bridgeCol = Math.min(col, cell.col);
                    return gameState.bridges.vertical[row][bridgeCol];
                }
            }
        }
        return false;
    });
}

// Move to the next player's turn
function nextTurn() {
    // Reset game phase
    gameState.gamePhase = 'select';
    gameState.selectedLutin = null;
    
    // Passer au joueur suivant avec modulo
    gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    
    // Si le joueur suivant n'est pas actif, passer au suivant
    while (!gameState.activePlayers.includes(PLAYERS[gameState.currentPlayer])) {
        gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    }
    
    // Mettre à jour l'interface
    renderBoard();
    updateGameInfo();
    
    // Gérer le tour du joueur suivant
    handlePlayerTurn();
}

// Simple AI for computer players
function playAITurn() {
    if (gameState.gameOver) return;
    
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    console.log(`IA joue pour le joueur: ${currentPlayer}`);
    
    try {
        // 1. Select a random lutin that can move
        const movableLutins = gameState.lutins[currentPlayer].filter(lutin => {
            return hasAvailableMoves(lutin.row, lutin.col);
        });
        
        console.log(`Nombre de lutins pouvant bouger: ${movableLutins.length}`);
        
        if (movableLutins.length === 0) {
            console.log(`L'IA ${currentPlayer} n'a pas de mouvements possibles, passe son tour`);
            
            // Vérifier si le joueur doit être éliminé
            checkPlayerElimination();
            
            // Passer au joueur suivant
            passerAuJoueurSuivant();
            return;
        }
        
        // Sélectionner un lutin aléatoire parmi ceux qui peuvent bouger
        const selectedLutin = movableLutins[Math.floor(Math.random() * movableLutins.length)];
        console.log(`Lutin sélectionné: [${selectedLutin.row}, ${selectedLutin.col}]`);
        
        // Mettre en évidence le lutin sélectionné
        const lutinElement = document.querySelector(`.lutin[data-row="${selectedLutin.row}"][data-col="${selectedLutin.col}"]`);
        if (lutinElement) {
            lutinElement.classList.add('highlight-lutin');
        }
        
        // Attendre 1.5 secondes pour que l'utilisateur voie le lutin sélectionné
        setTimeout(() => {
            // 2. Trouver tous les mouvements possibles pour ce lutin
            const possibleMoves = [];
            const adjacentCells = [
                { row: selectedLutin.row - 1, col: selectedLutin.col }, // Haut
                { row: selectedLutin.row + 1, col: selectedLutin.col }, // Bas
                { row: selectedLutin.row, col: selectedLutin.col - 1 }, // Gauche
                { row: selectedLutin.row, col: selectedLutin.col + 1 }  // Droite
            ];
            
            adjacentCells.forEach(cell => {
                if (cell.row >= 0 && cell.row < BOARD_SIZE && cell.col >= 0 && cell.col < BOARD_SIZE) {
                    if (isValidMove(selectedLutin.row, selectedLutin.col, cell.row, cell.col)) {
                        possibleMoves.push(cell);
                    }
                }
            });
            
            console.log(`Nombre de mouvements possibles: ${possibleMoves.length}`);
            
            if (possibleMoves.length === 0) {
                console.log(`Aucun mouvement possible pour ce lutin, passe au joueur suivant`);
                passerAuJoueurSuivant();
                return;
            }
            
            // 3. Choisir un mouvement aléatoire
            const targetCell = possibleMoves[Math.floor(Math.random() * possibleMoves.length)];
            console.log(`Mouvement choisi: [${targetCell.row}, ${targetCell.col}]`);
            
            // 4. Déplacer le lutin
            moveLutin(selectedLutin.row, selectedLutin.col, targetCell.row, targetCell.col);
            gameState.gamePhase = 'remove';
            
            // Mettre à jour l'interface
            renderBoard();
            updateGameInfo();
            
            // Mettre en évidence le lutin déplacé
            const movedLutinElement = document.querySelector(`.lutin[data-row="${targetCell.row}"][data-col="${targetCell.col}"]`);
            if (movedLutinElement) {
                movedLutinElement.classList.add('highlight-lutin');
            }
            
            // 5. Après 2 secondes, retirer un pont aléatoire
            setTimeout(() => {
                try {
                    // Récupérer tous les ponts disponibles
                    const availableBridges = [];
                    
                    // Vérifier les ponts horizontaux
                    for (let row = 0; row < BOARD_SIZE; row++) {
                        for (let col = 0; col < BOARD_SIZE; col++) {
                            if (gameState.bridges.horizontal[row][col]) {
                                availableBridges.push({ type: 'horizontal', row, col });
                            }
                        }
                    }
                    
                    // Vérifier les ponts verticaux
                    for (let row = 0; row < BOARD_SIZE; row++) {
                        for (let col = 0; col < BOARD_SIZE; col++) {
                            if (gameState.bridges.vertical[row][col]) {
                                availableBridges.push({ type: 'vertical', row, col });
                            }
                        }
                    }
                    
                    console.log(`Nombre de ponts disponibles: ${availableBridges.length}`);
                    
                    // Choisir un pont aléatoire à retirer
                    if (availableBridges.length > 0) {
                        const bridgeToRemove = availableBridges[Math.floor(Math.random() * availableBridges.length)];
                        
                        // Trouver l'élément du pont à retirer pour l'animation
                        let bridgeElement;
                        if (bridgeToRemove.type === 'horizontal') {
                            // Chercher le pont horizontal
                            const bridgeSelector = `.bridge-horizontal[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`;
                            bridgeElement = document.querySelector(bridgeSelector);
                            
                            // Si on ne trouve pas avec les coordonnées exactes, chercher les ponts top/bottom
                            if (!bridgeElement) {
                                const topSelector = `.bridge-horizontal.top[data-row="${bridgeToRemove.row + 1}"][data-col="${bridgeToRemove.col}"]`;
                                const bottomSelector = `.bridge-horizontal.bottom[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`;
                                bridgeElement = document.querySelector(topSelector) || document.querySelector(bottomSelector);
                            }
                        } else {
                            // Chercher le pont vertical
                            const bridgeSelector = `.bridge-vertical[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`;
                            bridgeElement = document.querySelector(bridgeSelector);
                            
                            // Si on ne trouve pas avec les coordonnées exactes, chercher les ponts left/right
                            if (!bridgeElement) {
                                const leftSelector = `.bridge-vertical.left[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col + 1}"]`;
                                const rightSelector = `.bridge-vertical.right[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`;
                                bridgeElement = document.querySelector(leftSelector) || document.querySelector(rightSelector);
                            }
                        }
                        
                        // Appliquer l'animation au pont avant de le supprimer
                        if (bridgeElement) {
                            bridgeElement.classList.add('highlight-bridge');
                            
                            // Attendre que l'animation se termine avant de supprimer réellement le pont
                            setTimeout(() => {
                                if (bridgeToRemove.type === 'horizontal') {
                                    gameState.bridges.horizontal[bridgeToRemove.row][bridgeToRemove.col] = false;
                                    console.log(`IA a retiré un pont ${bridgeToRemove.type} à la position [${bridgeToRemove.row}, ${bridgeToRemove.col}]`);
                                } else {
                                    gameState.bridges.vertical[bridgeToRemove.row][bridgeToRemove.col] = false;
                                    console.log(`IA a retiré un pont ${bridgeToRemove.type} à la position [${bridgeToRemove.row}, ${bridgeToRemove.col}]`);
                                }
                                
                                // Vérifier si un joueur est éliminé
                                checkPlayerElimination();
                                
                                // Passer au joueur suivant après 1 seconde
                                setTimeout(() => {
                                    passerAuJoueurSuivant();
                                }, 1000);
                            }, 1500);
                        } else {
                            // Si on ne trouve pas le pont dans le DOM, le supprimer directement
                            if (bridgeToRemove.type === 'horizontal') {
                                gameState.bridges.horizontal[bridgeToRemove.row][bridgeToRemove.col] = false;
                            } else {
                                gameState.bridges.vertical[bridgeToRemove.row][bridgeToRemove.col] = false;
                            }
                            
                            // Vérifier si un joueur est éliminé
                            checkPlayerElimination();
                            
                            // Mettre à jour l'interface
                            renderBoard();
                            
                            // Passer au joueur suivant
                            setTimeout(() => {
                                passerAuJoueurSuivant();
                            }, 1000);
                        }
                    } else {
                        console.log("Aucun pont disponible à retirer");
                        
                        // Passer au joueur suivant
                        setTimeout(() => {
                            passerAuJoueurSuivant();
                        }, 1000);
                    }
                } catch (error) {
                    console.error("Erreur lors de la suppression du pont:", error);
                    passerAuJoueurSuivant();
                }
            }, 2000);
        }, 1500);
    } catch (error) {
        console.error("Erreur dans le tour de l'IA:", error);
        passerAuJoueurSuivant();
    }
}

// Fonction pour passer au joueur suivant
function passerAuJoueurSuivant() {
    // Réinitialiser la phase du jeu
    gameState.gamePhase = 'select';
    gameState.selectedLutin = null;
    
    // Passer au joueur suivant avec modulo
    gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    
    // Si le joueur suivant n'est pas actif, passer au suivant
    while (!gameState.activePlayers.includes(PLAYERS[gameState.currentPlayer])) {
        gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    }
    
    // Mettre à jour l'interface
    renderBoard();
    updateGameInfo();
    
    // Vérifier si le joueur suivant est humain ou IA
    const nextPlayer = PLAYERS[gameState.currentPlayer];
    console.log(`Passage au joueur suivant: ${nextPlayer}`);
    
    if (AI_PLAYERS.includes(nextPlayer) && !gameState.gameOver) {
        setTimeout(playAITurn, 3000);
    } else {
        enableHumanPlayerLutins();
    }
}

// Event listeners
resetButton.addEventListener('click', () => {
    initGame();
});

helpButton.addEventListener('click', () => {
    helpModal.style.display = 'block';
});

closeModalButton.addEventListener('click', () => {
    helpModal.style.display = 'none';
});

window.addEventListener('click', (event) => {
    if (event.target === helpModal) {
        helpModal.style.display = 'none';
    }
});

// Initialize the game when the page loads
window.addEventListener('DOMContentLoaded', initGame);

// Get CSS color for player
function getPlayerColor(player) {
    switch (player) {
        case 'green': return '#2ecc71';
        case 'blue': return '#3498db';
        case 'yellow': return '#f1c40f';
        case 'red': return '#e74c3c';
        default: return '#333';
    }
}
