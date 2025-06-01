/**
 * PontuXL Game Logic - Corrected Version
 * This file contains the main game logic for the PontuXL game
 * L'IA utilise l'algorithme Maxⁿ avec élagage superficiel implémenté en Prolog
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
    try {
        // Create the board structure
        createBoard();
        
        // Initialize game state
        resetGameState();
        
        // Place lutins on the board
        placeLutins();
        
        // Render the initial board state
        renderBoard();

        // Update game info
        updateGameInfo();

        // Enable interactions for human players
        if (HUMAN_PLAYERS.includes(PLAYERS[gameState.currentPlayer])) {
            enableHumanPlayerLutins();
        }

        // Clear any error messages
        if (gameMessage) {
            gameMessage.textContent = '';
            gameMessage.style.color = '';
            gameMessage.style.fontWeight = '';
            gameMessage.style.fontSize = '';
        }
    } catch (error) {
        console.error('Erreur lors de l\'initialisation du jeu:', error);
        if (gameMessage) {
            gameMessage.textContent = 'Erreur de chargement. Veuillez rafraîchir la page.';
            gameMessage.style.color = 'red';
        }
    }
}

// Create the board structure
function createBoard() {
    if (!gameBoard) {
        throw new Error('Élément game-board non trouvé dans le DOM');
    }

    try {
        gameBoard.innerHTML = '';
        
        // Create cells
        for (let row = 0; row < BOARD_SIZE; row++) {
            for (let col = 0; col < BOARD_SIZE; col++) {
                try {
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
                } catch (error) {
                    console.error(`Erreur lors de la création de la cellule [${row},${col}]:`, error);
                    throw error;
                }
            }
        }
    } catch (error) {
        console.error('Erreur lors de la création du plateau:', error);
        throw error;
    }
}

// Reset game state
function resetGameState() {
    // Initialize board
    gameState.board = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE).fill(null));
    
    // Initialize bridges - CORRECTED: proper dimensions for bridges
    gameState.bridges.horizontal = Array(BOARD_SIZE - 1).fill().map(() => Array(BOARD_SIZE).fill(true));
    gameState.bridges.vertical = Array(BOARD_SIZE).fill().map(() => Array(BOARD_SIZE - 1).fill(true));
    
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
                    lutin.style.cursor = 'pointer';
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
                    } else {
                        bridge.style.cursor = 'default';
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
                    } else {
                        bridge.style.cursor = 'default';
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
                    } else {
                        bridge.style.cursor = 'default';
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
                    } else {
                        bridge.style.cursor = 'default';
                    }
                }
            }
        }
    }
}

// Update game information display
function updateGameInfo() {
    if (!currentPlayerDisplay || !gameMessage) {
        return;
    }
    
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
        
        // Vérifier que les coordonnées sont valides
        if (type === 'horizontal' && bridgeRow >= 0 && bridgeRow < BOARD_SIZE - 1 && bridgeCol >= 0 && bridgeCol < BOARD_SIZE) {
            gameState.bridges.horizontal[bridgeRow][bridgeCol] = false;
            console.log(`Pont horizontal supprimé à [${bridgeRow}, ${bridgeCol}]`);
        } else if (type === 'vertical' && bridgeRow >= 0 && bridgeRow < BOARD_SIZE && bridgeCol >= 0 && bridgeCol < BOARD_SIZE - 1) {
            gameState.bridges.vertical[bridgeRow][bridgeCol] = false;
            console.log(`Pont vertical supprimé à [${bridgeRow}, ${bridgeCol}]`);
        } else {
            console.log("Coordonnées de pont invalides, suppression annulée");
            return;
        }
        
        // Vérifier si un joueur est éliminé
        checkPlayerElimination();
        
        // Passer au joueur suivant
        nextTurn();
    } else {
        console.log(`Phase actuelle: ${gameState.gamePhase}, pas en phase de suppression de pont`);
    }
}

// Check if a move is valid
function isValidMove(fromRow, fromCol, toRow, toCol) {
    // Check bounds
    if (toRow < 0 || toRow >= BOARD_SIZE || toCol < 0 || toCol >= BOARD_SIZE) {
        return false;
    }
    
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
    let eliminatedPlayers = [];
    
    PLAYERS.forEach(player => {
        if (gameState.activePlayers.includes(player)) {
            const isEliminated = gameState.lutins[player].every(lutin => {
                return !hasAvailableMoves(lutin.row, lutin.col);
            });
            
            if (isEliminated) {
                eliminatedPlayers.push(player);
                console.log(`Le joueur ${player} est éliminé!`);
            }
        }
    });
    
    // Remove eliminated players from active players
    eliminatedPlayers.forEach(player => {
        gameState.activePlayers = gameState.activePlayers.filter(p => p !== player);
    });
    
    // Check if game is over
    if (gameState.activePlayers.length === 1) {
        const winner = gameState.activePlayers[0];
        gameState.gameOver = true;
        
        // Afficher le message de victoire
        if (gameMessage) {
            gameMessage.textContent = `Le joueur ${winner.charAt(0).toUpperCase() + winner.slice(1)} a gagné la partie!`;
            gameMessage.style.color = getPlayerColor(winner);
            gameMessage.style.fontWeight = 'bold';
            gameMessage.style.fontSize = '1.5rem';
        }
        
        // Désactiver les interactions avec le plateau
        disableAllInteractions();
        
        // Afficher une boîte de dialogue de victoire
        showVictoryMessage(winner);
        
        console.log(`Le jeu est terminé! Le joueur ${winner} a gagné!`);
        return true;
    }
    
    return false;
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
    
    // Move to next player
    gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    
    // Skip eliminated players
    while (!gameState.activePlayers.includes(PLAYERS[gameState.currentPlayer])) {
        gameState.currentPlayer = (gameState.currentPlayer + 1) % PLAYERS.length;
    }
    
    // Mettre à jour l'interface
    renderBoard();
    updateGameInfo();
    
    // Gérer le tour du joueur suivant
    handlePlayerTurn();
}

// Utilise directement l'IA basique pour éviter les blocages
async function playAITurn() {
    if (gameState.gameOver) return;
    
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    console.log(`IA joue pour le joueur: ${currentPlayer}`);
    
    // Créer une promesse qui sera résolue soit par l'IA, soit par le timeout
    const aiPromise = new Promise((resolve) => {
        // Essayer d'utiliser l'IA Prolog
        try {
            // Appeler l'IA Prolog ici
            resolve(useBasicAI());
        } catch (error) {
            console.error('Erreur avec l\'IA Prolog:', error);
            resolve(false);
        }
    });
    
    // Créer une promesse de timeout
    const timeoutPromise = new Promise((resolve) => {
        setTimeout(() => {
            resolve('timeout');
        }, 5000); // 5 secondes
    });
    
    // Utiliser Promise.race pour voir quelle promesse se termine en premier
    const result = await Promise.race([aiPromise, timeoutPromise]);
    
    if (result === 'timeout') {
        console.log('L\'IA a pris trop de temps, on joue un coup aléatoire');
        // Jouer un coup aléatoire
        playRandomMove();
    }
}

// Fonction pour jouer un coup aléatoire
function playRandomMove() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    const lutins = gameState.lutins[currentPlayer];
    
    // Trouver un lutin qui peut bouger
    let validLutin = null;
    let validMoves = [];
    
    for (const lutin of lutins) {
        const moves = getValidMoves(lutin.row, lutin.col);
        if (moves.length > 0) {
            validLutin = lutin;
            validMoves = moves;
            break;
        }
    }
    
            if (validLutin && validMoves.length > 0) {
        // Choisir un mouvement aléatoire
        const randomMove = validMoves[Math.floor(Math.random() * validMoves.length)];
        
        // Déplacer le lutin
        moveLutin(validLutin.row, validLutin.col, randomMove.row, randomMove.col);
        
        // Supprimer un pont aléatoire
        const bridges = getAvailableBridges();
        if (bridges.length > 0) {
            const randomBridge = bridges[Math.floor(Math.random() * bridges.length)];
            if (randomBridge.type === 'horizontal') {
                gameState.bridges.horizontal[randomBridge.row][randomBridge.col] = false;
            } else {
                gameState.bridges.vertical[randomBridge.row][randomBridge.col] = false;
            }
        }
        
        // Vérifier si un joueur est éliminé
        checkPlayerElimination();
        
        // Mettre à jour l'interface
        renderBoard();
        updateGameInfo();
        
        // Passer au joueur suivant
        nextTurn();
    }
}

// Fonction pour obtenir tous les ponts disponibles
function getAvailableBridges() {
    const bridges = [];
    
    // Vérifier les ponts horizontaux
    for (let row = 0; row < BOARD_SIZE - 1; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            if (gameState.bridges.horizontal[row][col]) {
                bridges.push({ type: 'horizontal', row, col });
            }
        }
    }
    
    // Vérifier les ponts verticaux
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE - 1; col++) {
            if (gameState.bridges.vertical[row][col]) {
                bridges.push({ type: 'vertical', row, col });
            }
        }
    }
    
    return bridges;
}

// Fonction pour obtenir les mouvements valides pour un lutin
function getValidMoves(row, col) {
    const moves = [];
    
    // Vérifier les cases adjacentes
    const directions = [
        { dr: -1, dc: 0 }, // haut
        { dr: 1, dc: 0 },  // bas
        { dr: 0, dc: -1 }, // gauche
        { dr: 0, dc: 1 }   // droite
    ];
    
    for (const dir of directions) {
        const newRow = row + dir.dr;
        const newCol = col + dir.dc;
        
        if (isValidMove(row, col, newRow, newCol)) {
            moves.push({ row: newRow, col: newCol });
        }
    }
    
    return moves;
}

// Fonction pour exécuter un mouvement de l'IA
function executeAIMove(selectedLutin, targetCell, bridgeToRemove) {
    // D'abord, montrer le lutin sélectionné
    gameState.selectedLutin = selectedLutin;
    gameState.gamePhase = 'move';
    renderBoard();
    updateGameInfo();
    
    // Message indiquant le mouvement de l'IA
    if (gameMessage) {
        gameMessage.textContent = `L'IA déplace un lutin de [${selectedLutin.row},${selectedLutin.col}] vers [${targetCell.row},${targetCell.col}]`;
        gameMessage.style.color = getPlayerColor(PLAYERS[gameState.currentPlayer]);
        gameMessage.style.fontWeight = 'bold';
    }
    
    // Attendre 1.5 secondes pour montrer la sélection
    setTimeout(() => {
        // Déplacer le lutin avec animation visuelle
        const selectedLutinElement = document.querySelector(`.lutin[data-row="${selectedLutin.row}"][data-col="${selectedLutin.col}"]`);
        const targetCellElement = document.querySelector(`.cell[data-row="${targetCell.row}"][data-col="${targetCell.col}"]`);
        
        if (selectedLutinElement && targetCellElement) {
            // Ajouter une classe d'animation pour le mouvement
            selectedLutinElement.style.transition = 'all 0.8s ease-in-out';
            selectedLutinElement.style.transform = 'scale(1.2)';
            selectedLutinElement.style.zIndex = '10';
            selectedLutinElement.style.boxShadow = '0 0 20px rgba(255, 255, 0, 0.8)';
        }
        
        // Attendre l'animation puis déplacer
        setTimeout(() => {
            // Déplacer le lutin
            moveLutin(selectedLutin.row, selectedLutin.col, targetCell.row, targetCell.col);
            
            // Passer à la phase de suppression de pont
            gameState.gamePhase = 'remove';
            
            // Mettre à jour l'interface
            renderBoard();
            
            // Message pour la suppression du pont
            if (gameMessage) {
                gameMessage.textContent = `L'IA supprime un pont ${bridgeToRemove.type} à [${bridgeToRemove.row},${bridgeToRemove.col}]`;
            }
            
            // Attendre 2 secondes pour que l'utilisateur voie le déplacement
            setTimeout(() => {
                try {
                    // Supprimer le pont avec animation
                    let bridgeElement = null;
                    
                    // Trouver l'élément du pont à supprimer
                    if (bridgeToRemove.type === 'horizontal') {
                        bridgeElement = document.querySelector(`.bridge-horizontal[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`) ||
                                      document.querySelector(`.bridge-horizontal.bottom[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`) ||
                                      document.querySelector(`.bridge-horizontal.top[data-row="${bridgeToRemove.row + 1}"][data-col="${bridgeToRemove.col}"]`);
                    } else {
                        bridgeElement = document.querySelector(`.bridge-vertical[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`) ||
                                      document.querySelector(`.bridge-vertical.right[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col}"]`) ||
                                      document.querySelector(`.bridge-vertical.left[data-row="${bridgeToRemove.row}"][data-col="${bridgeToRemove.col + 1}"]`);
                    }
                    
                    // Animation de suppression du pont
                    if (bridgeElement) {
                        bridgeElement.style.transition = 'all 0.5s ease-out';
                        bridgeElement.style.backgroundColor = '#e74c3c';
                        bridgeElement.style.transform = 'scale(1.5)';
                        bridgeElement.style.opacity = '0.8';
                        
                        setTimeout(() => {
                            bridgeElement.style.opacity = '0';
                            bridgeElement.style.transform = 'scale(0)';
                        }, 300);
                    }
                    
                    // Supprimer le pont de l'état du jeu
                    if (bridgeToRemove.type === 'horizontal') {
                        gameState.bridges.horizontal[bridgeToRemove.row][bridgeToRemove.col] = false;
                        console.log(`Pont horizontal supprimé à [${bridgeToRemove.row}, ${bridgeToRemove.col}]`);
                    } else {
                        gameState.bridges.vertical[bridgeToRemove.row][bridgeToRemove.col] = false;
                        console.log(`Pont vertical supprimé à [${bridgeToRemove.row}, ${bridgeToRemove.col}]`);
                    }
                    
                    // Mettre à jour l'interface pour refléter la suppression du pont
                    setTimeout(() => {
                        renderBoard();
                        
                        // Vérifier si un joueur est éliminé
                        checkPlayerElimination();
                        
                        // Passer au joueur suivant après 1 seconde
                        setTimeout(() => {
                            if (gameMessage) {
                                gameMessage.style.fontWeight = 'normal';
                                gameMessage.style.color = '';
                            }
                            nextTurn();
                        }, 1000);
                    }, 800);
                    
                } catch (error) {
                    console.error("Erreur lors de la suppression du pont:", error);
                    nextTurn();
                }
            }, 2000);
        }, 800);
    }, 1500);
}

// Fonction pour utiliser l'IA basique en cas d'échec de l'IA Prolog
function useBasicAI() {
    const currentPlayer = PLAYERS[gameState.currentPlayer];
    
    // Trouver les lutins qui peuvent se déplacer
    const movableLutins = gameState.lutins[currentPlayer].filter(lutin => {
        return hasAvailableMoves(lutin.row, lutin.col);
    });
    
    if (movableLutins.length === 0) {
        console.log(`L'IA ${currentPlayer} n'a pas de mouvements possibles, passe son tour`);
        checkPlayerElimination();
        nextTurn();
        return;
    }
    
    // Sélectionner un lutin aléatoire qui peut se déplacer
    const selectedLutin = movableLutins[Math.floor(Math.random() * movableLutins.length)];
    
    // Trouver toutes les destinations possibles
    const possibleMoves = [];
    const directions = [
        { row: -1, col: 0 }, // Haut
        { row: 1, col: 0 },  // Bas
        { row: 0, col: -1 }, // Gauche
        { row: 0, col: 1 }   // Droite
    ];
    
    for (const dir of directions) {
        const targetRow = selectedLutin.row + dir.row;
        const targetCol = selectedLutin.col + dir.col;
        
        if (isValidMove(selectedLutin.row, selectedLutin.col, targetRow, targetCol)) {
            possibleMoves.push({ row: targetRow, col: targetCol });
        }
    }
    
    // Sélectionner une destination aléatoire
    const targetCell = possibleMoves[Math.floor(Math.random() * possibleMoves.length)];
    
    // Trouver tous les ponts disponibles
    const availableBridges = getAvailableBridges();
    
    // Sélectionner un pont aléatoire
    const bridgeToRemove = availableBridges[Math.floor(Math.random() * availableBridges.length)];
    
    // Exécuter le mouvement
    executeAIMove(selectedLutin, targetCell, bridgeToRemove);
}

// Event listeners
if (resetButton) {
    resetButton.addEventListener('click', () => {
        initGame();
    });
}

if (helpButton && helpModal) {
    helpButton.addEventListener('click', () => {
        helpModal.style.display = 'block';
    });
}

if (closeModalButton && helpModal) {
    closeModalButton.addEventListener('click', () => {
        helpModal.style.display = 'none';
    });
}

window.addEventListener('click', (event) => {
    if (helpModal && event.target === helpModal) {
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

// Désactiver toutes les interactions avec le plateau de jeu
function disableAllInteractions() {
    // Désactiver les lutins
    document.querySelectorAll('.lutin').forEach(lutin => {
        lutin.style.cursor = 'default';
        
        // Cloner pour supprimer les événements
        const newLutin = lutin.cloneNode(true);
        lutin.parentNode.replaceChild(newLutin, lutin);
    });
    
    // Désactiver les ponts
    document.querySelectorAll('.bridge-horizontal, .bridge-vertical').forEach(bridge => {
        bridge.style.cursor = 'default';
        
        // Cloner pour supprimer les événements
        const newBridge = bridge.cloneNode(true);
        bridge.parentNode.replaceChild(newBridge, bridge);
    });
    
    // Désactiver les cellules
    document.querySelectorAll('.cell').forEach(cell => {
        cell.style.cursor = 'default';
        
        // Cloner pour supprimer les événements
        const newCell = cell.cloneNode(true);
        cell.parentNode.replaceChild(newCell, cell);
    });
}

// Afficher un message de victoire plus visible
function showVictoryMessage(winner) {
    try {
        // Désactiver toutes les interactions
        disableAllInteractions();
        
        // Mettre à jour l'état du jeu
        gameState.gameOver = true;
        updateGameInfo();
        
        console.log(`Victoire du joueur ${winner}`);
        
        // Créer l'overlay de victoire avec centrage parfait
        const victoryOverlay = document.createElement('div');
        victoryOverlay.classList.add('victory-message');
        victoryOverlay.style.position = 'fixed';
        victoryOverlay.style.top = '0';
        victoryOverlay.style.left = '0';
        victoryOverlay.style.width = '100vw';
        victoryOverlay.style.height = '100vh';
        victoryOverlay.style.backgroundColor = 'rgba(0, 0, 0, 0.85)';
        victoryOverlay.style.display = 'flex';
        victoryOverlay.style.justifyContent = 'center';
        victoryOverlay.style.alignItems = 'center';
        victoryOverlay.style.zIndex = '9999';
        victoryOverlay.style.margin = '0';
        victoryOverlay.style.padding = '0';
        
        // Créer le contenu avec animation d'apparition
        const victoryContent = document.createElement('div');
        victoryContent.classList.add('victory-content');
        victoryContent.style.backgroundColor = 'white';
        victoryContent.style.padding = '40px 50px';
        victoryContent.style.borderRadius = '15px';
        victoryContent.style.textAlign = 'center';
        victoryContent.style.boxShadow = '0 10px 30px rgba(0, 0, 0, 0.3)';
        victoryContent.style.maxWidth = '90vw';
        victoryContent.style.maxHeight = '90vh';
        victoryContent.style.transform = 'scale(0.8)';
        victoryContent.style.transition = 'transform 0.3s ease-out';
        victoryContent.style.border = `4px solid ${getPlayerColor(winner)}`;
        
        // Animation d'entrée
        setTimeout(() => {
            victoryContent.style.transform = 'scale(1)';
        }, 10);
        
        // Créer le titre avec animation
        const victoryTitle = document.createElement('h2');
        victoryTitle.textContent = '🎉 VICTOIRE ! 🎉';
        victoryTitle.style.margin = '0 0 25px 0';
        victoryTitle.style.fontSize = '2.5em';
        victoryTitle.style.color = getPlayerColor(winner);
        victoryTitle.style.textShadow = '2px 2px 4px rgba(0, 0, 0, 0.2)';
        victoryTitle.style.fontFamily = 'Arial, sans-serif';
        victoryTitle.style.fontWeight = 'bold';
        
        // Créer le message du gagnant
        const victoryMessage = document.createElement('p');
        victoryMessage.textContent = `Le joueur ${winner.charAt(0).toUpperCase() + winner.slice(1)} a gagné la partie !`;
        victoryMessage.style.color = getPlayerColor(winner);
        victoryMessage.style.fontSize = '1.8em';
        victoryMessage.style.fontWeight = 'bold';
        victoryMessage.style.margin = '0 0 30px 0';
        victoryMessage.style.fontFamily = 'Arial, sans-serif';
        
        // Créer le bouton pour rejouer avec effet hover
        const replayButton = document.createElement('button');
        replayButton.textContent = '🎮 Nouvelle Partie';
        replayButton.style.padding = '15px 30px';
        replayButton.style.fontSize = '1.3em';
        replayButton.style.backgroundColor = getPlayerColor(winner);
        replayButton.style.color = 'white';
        replayButton.style.border = 'none';
        replayButton.style.borderRadius = '8px';
        replayButton.style.cursor = 'pointer';
        replayButton.style.fontWeight = 'bold';
        replayButton.style.fontFamily = 'Arial, sans-serif';
        replayButton.style.transition = 'all 0.3s ease';
        replayButton.style.boxShadow = '0 4px 8px rgba(0, 0, 0, 0.2)';
        
        // Effets hover pour le bouton
        replayButton.addEventListener('mouseenter', () => {
            replayButton.style.transform = 'translateY(-2px)';
            replayButton.style.boxShadow = '0 6px 12px rgba(0, 0, 0, 0.3)';
        });
        
        replayButton.addEventListener('mouseleave', () => {
            replayButton.style.transform = 'translateY(0)';
            replayButton.style.boxShadow = '0 4px 8px rgba(0, 0, 0, 0.2)';
        });
        
        replayButton.addEventListener('click', () => {
            // Animation de sortie
            victoryContent.style.transform = 'scale(0.8)';
            victoryOverlay.style.opacity = '0';
            
            setTimeout(() => {
                if (document.body.contains(victoryOverlay)) {
                    document.body.removeChild(victoryOverlay);
                }
                initGame();
            }, 200);
        });
        
        // Créer un bouton de fermeture (X)
        const closeButton = document.createElement('button');
        closeButton.textContent = '✕';
        closeButton.style.position = 'absolute';
        closeButton.style.top = '10px';
        closeButton.style.right = '15px';
        closeButton.style.background = 'none';
        closeButton.style.border = 'none';
        closeButton.style.fontSize = '1.5em';
        closeButton.style.color = '#666';
        closeButton.style.cursor = 'pointer';
        closeButton.style.width = '30px';
        closeButton.style.height = '30px';
        closeButton.style.borderRadius = '50%';
        closeButton.style.transition = 'all 0.2s ease';
        
        closeButton.addEventListener('mouseenter', () => {
            closeButton.style.backgroundColor = '#f0f0f0';
            closeButton.style.color = '#000';
        });
        
        closeButton.addEventListener('mouseleave', () => {
            closeButton.style.backgroundColor = 'transparent';
            closeButton.style.color = '#666';
        });
        
        closeButton.addEventListener('click', () => {
            // Animation de sortie
            victoryContent.style.transform = 'scale(0.8)';
            victoryOverlay.style.opacity = '0';
            
            setTimeout(() => {
                if (document.body.contains(victoryOverlay)) {
                    document.body.removeChild(victoryOverlay);
                }
                initGame();
            }, 200);
        });
        
        // Permettre de fermer avec la touche Escape
        const escapeHandler = (event) => {
            if (event.key === 'Escape') {
                closeButton.click();
                document.removeEventListener('keydown', escapeHandler);
            }
        };
        document.addEventListener('keydown', escapeHandler);
        
        // Assembler le contenu
        victoryContent.style.position = 'relative';
        victoryContent.appendChild(closeButton);
        victoryContent.appendChild(victoryTitle);
        victoryContent.appendChild(victoryMessage);
        victoryContent.appendChild(replayButton);
        victoryOverlay.appendChild(victoryContent);
        
        // Ajouter l'overlay au body
        document.body.appendChild(victoryOverlay);
        
        // Focus sur le bouton pour permettre l'interaction clavier
        setTimeout(() => {
            replayButton.focus();
        }, 100);
        
    } catch (error) {
        console.error('Erreur lors de l\'affichage du message de victoire:', error);
    }
}