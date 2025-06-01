/**
 * PontuXL Bot Interface
 * This file handles the communication between the web interface and the Prolog bot
 */

// DOM elements
const chatMessages = document.getElementById('chat-messages');
const userInput = document.getElementById('user-input');
const sendButton = document.getElementById('send-button');

// Sample responses for the bot (will be replaced with Prolog integration)
// Questions et réponses du chatbot
const gameQuestions = [
    {
        keywords: ['hello', 'bonjour', 'salut', 'coucou', 'bonsoir', 'hey', 'hi', 'hola', 'yo', 'ola', 'slt'],
        response: "Bonjour ! Je suis PBot, le bot explicateur du jeu PontuXL. En quoi puis-je vous être utile ?"
    },
    {
        keywords: ['commence', 'premier', 'debute', 'debut', 'debut partie', 'qui commence', 'qui commence le jeu'],
        response: "Par convention, c'est au joueur en charge des lutins verts de commencer la partie."
    },
    {
        keywords: ['combien', 'nombre', 'lutin', 'pion', 'lutins', 'pions', 'equipe', 'équipe'],
        response: "Chaque équipe compte 4 lutins."
    },
    {
        keywords: ['gagner', 'victoire', 'gagne', 'but', 'but du jeu', 'comment gagner', 'fin partie', 'quand fin partie'],
        response: "La partie se termine lorsqu'il ne reste plus qu'un seul joueur dont les lutins ne sont pas tous isolés. Ce joueur est déclaré vainqueur."
    },
    {
        keywords: ['jouer', 'joue', 'deroulement', 'tour', 'comment', 'comment jouer', 'coment', 'comman', 'commant', 'commente', 'deplacer', 'déplacer'],
        response: "Pour déplacer un lutin, vous devez le faire passer d'une case à une autre en empruntant un pont. Deux lutins ne peuvent pas se trouver sur la même case. Après avoir déplacé un lutin, vous devez retirer un pont ou tourner un pont d'un quart de tour."
    },
    {
        keywords: ['règle', 'règles', 'principe', 'base'],
        response: "Pontu est un jeu où vous déplacez vos lutins et retirez des ponts pour isoler les lutins adverses. Un joueur est éliminé quand tous ses lutins sont isolés."
    },
    {
        keywords: ['déplacer', 'mouvement', 'bouger', 'déplacement'],
        response: "Vous pouvez déplacer un lutin vers une case adjacente en passant par un pont. Deux lutins ne peuvent pas occuper la même case."
    },
    {
        keywords: ['retirer', 'enlever', 'supprimer', 'pont'],
        response: "Après avoir déplacé un lutin, vous devez retirer un pont. Vous pouvez retirer le pont que vous venez d'emprunter ou n'importe quel autre pont."
    },
    {
        keywords: ['tourner', 'rotation', 'pivoter', 'orienter'],
        response: "Au lieu de retirer un pont, vous pouvez choisir de tourner un pont d'un quart de tour."
    },
    {
        keywords: ['taille', 'dimension', 'plateau', 'grille'],
        response: "Dans PontuXL, le plateau est de taille 6x6."
    },
    {
        keywords: ['ordre', 'tour', 'sequence', 'joueur'],
        response: "Les joueurs jouent dans l'ordre suivant : vert, bleu, jaune, rouge."
    },
    {
        keywords: ['ia', 'intelligence', 'artificielle', 'ordinateur', 'robot'],
        response: "Dans cette version du jeu, les lutins bleus et rouges sont contrôlés par une intelligence artificielle."
    },
    {
        keywords: ['isoler', 'éliminer', 'élimination'],
        response: "Un joueur est éliminé quand tous ses lutins sont isolés, c'est-à-dire qu'ils n'ont plus aucun pont accessible autour d'eux."
    },
    {
        keywords: ['couleur', 'équipe', 'camp'],
        response: "Il y a quatre équipes dans le jeu : verte, bleue, jaune et rouge. Les lutins verts et jaunes sont contrôlés par les joueurs humains."
    },
    {
        keywords: ['difficulté', 'niveau', 'ia'],
        response: "L'intelligence artificielle utilise un algorithme sophistiqué (Maxⁿ) pour jouer de manière stratégique, mais elle a un temps limité pour prendre ses décisions."
    },
    {
        keywords: ['stratégie', 'conseil', 'astuce', 'tactique'],
        response: "Quelques conseils stratégiques : gardez vos lutins groupés, essayez d'isoler les lutins adverses un par un, et pensez à plusieurs coups à l'avance."
    }
];

// Initialize the bot
function initBot() {
    // Get DOM elements
    const chatMessages = document.getElementById('chat-messages');
    const userInput = document.getElementById('user-input');
    const sendButton = document.getElementById('send-button');

    // Verify that all elements exist
    if (!chatMessages || !userInput || !sendButton) {
        console.error('Chat elements not found!');
        return;
    }

    // Store elements in window object to make them globally accessible
    window.chatElements = {
        chatMessages,
        userInput,
        sendButton
    };

    // Add event listeners
    sendButton.addEventListener('click', handleUserMessage);
    userInput.addEventListener('keypress', (event) => {
        if (event.key === 'Enter') {
            handleUserMessage();
        }
    });

    // Add initial bot message
    addMessage("Bonjour ! Je suis le chatbot de Pontu. Je peux vous aider avec les règles du jeu, comment jouer, et répondre à vos questions. Que voulez-vous savoir ?", 'bot');
}

// Handle user message
async function handleUserMessage() {
    const { userInput, chatMessages } = window.chatElements;
    const message = userInput.value.trim();
    
    if (message === '') return;
    
    // Display user message
    addMessage(message, 'user');
    
    // Clear input field
    userInput.value = '';
    
    try {
        // Process the message and get bot response using Prolog
        const response = await getPrologResponse(message);
        addMessage(response, 'bot');
    } catch (error) {
        console.error('Error getting response:', error);
        // Fallback to JavaScript version if Prolog fails
        const response = getBotResponse(message);
        addMessage(response, 'bot');
    }
    
    // Scroll to bottom
    chatMessages.scrollTop = chatMessages.scrollHeight;
}

// Add a message to the chat
function addMessage(message, sender) {
    const { chatMessages } = window.chatElements;
    
    if (!chatMessages) {
        console.error('Chat messages container not found!');
        return;
    }

    const messageElement = document.createElement('div');
    messageElement.className = sender === 'user' ? 'user-message' : 'bot-message';
    messageElement.textContent = message;
    
    chatMessages.appendChild(messageElement);
    
    // Scroll to bottom
    chatMessages.scrollTop = chatMessages.scrollHeight;
}

// Fonction pour calculer la distance de Levenshtein (similarité entre deux chaînes)
function levenshteinDistance(str1, str2) {
    const m = str1.length;
    const n = str2.length;
    const dp = Array.from({ length: m + 1 }, () => Array(n + 1).fill(0));

    for (let i = 0; i <= m; i++) dp[i][0] = i;
    for (let j = 0; j <= n; j++) dp[0][j] = j;

    for (let i = 1; i <= m; i++) {
        for (let j = 1; j <= n; j++) {
            if (str1[i - 1] === str2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1];
            } else {
                dp[i][j] = 1 + Math.min(
                    dp[i - 1][j],     // suppression
                    dp[i][j - 1],     // insertion
                    dp[i - 1][j - 1]   // remplacement
                );
            }
        }
    }
    return dp[m][n];
}

// Liste des salutations courtes qui doivent correspondre exactement
const shortGreetings = new Set(['hi', 'hey', 'yo', 'ola']);

// Fonction pour vérifier si deux mots sont similaires
function areSimilar(word1, word2, threshold = 0.5) {
    // Normaliser les mots
    word1 = word1.toLowerCase()
        .replace(/['']/g, "'")
        .replace(/[\u00e0\u00e2]/g, "a")
        .replace(/[\u00e8\u00e9\u00ea\u00eb]/g, "e")
        .replace(/[\u00ee\u00ef]/g, "i")
        .replace(/[\u00f4]/g, "o")
        .replace(/[\u00f9\u00fb]/g, "u")
        .replace(/[\u00e7]/g, "c")
        .trim();
    
    word2 = word2.toLowerCase()
        .replace(/['']/g, "'")
        .replace(/[\u00e0\u00e2]/g, "a")
        .replace(/[\u00e8\u00e9\u00ea\u00eb]/g, "e")
        .replace(/[\u00ee\u00ef]/g, "i")
        .replace(/[\u00f4]/g, "o")
        .replace(/[\u00f9\u00fb]/g, "u")
        .replace(/[\u00e7]/g, "c")
        .trim();

    // Vérification spéciale pour les salutations courtes
    if (shortGreetings.has(word1) || shortGreetings.has(word2)) {
        return word1 === word2;
    }

    // Si les mots sont identiques après normalisation
    if (word1 === word2) {
        return true;
    }

    // Si un mot contient l'autre
    if (word1.includes(word2) || word2.includes(word1)) {
        return true;
    }

    // Pour les mots très courts (2 caractères ou moins)
    if (word1.length <= 2 || word2.length <= 2) {
        return false;
    }
    
    const distance = levenshteinDistance(word1, word2);
    const maxLength = Math.max(word1.length, word2.length);
    const similarity = 1 - distance / maxLength;
    
    return similarity >= threshold;
}

// Get bot response based on user input
function getBotResponse(message) {
    // Nettoyer et normaliser le message
    const normalizedMessage = message.toLowerCase()
        .replace(/['']/g, "'")
        .replace(/[\u00e0\u00e2]/g, "a")
        .replace(/[\u00e8\u00e9\u00ea\u00eb]/g, "e")
        .replace(/[\u00ee\u00ef]/g, "i")
        .replace(/[\u00f4]/g, "o")
        .replace(/[\u00f9\u00fb]/g, "u")
        .replace(/[\u00e7]/g, "c")
        .replace(/[^a-z0-9\s]/g, ' ')
        .trim();
    
    // Si le message est vide
    if (!normalizedMessage) {
        return "Je ne peux pas répondre à un message vide. Posez-moi une question sur le jeu !";
    }

    // Découper le message en mots
    const messageWords = normalizedMessage.split(/\s+/);
    
    // Créer des phrases de toutes les tailles possibles (jusqu'à 4 mots)
    const messagePhrases = [];
    for (let size = 1; size <= 4; size++) {
        for (let i = 0; i <= messageWords.length - size; i++) {
            messagePhrases.push(messageWords.slice(i, i + size).join(' '));
        }
    }
    
    // Structure pour stocker les réponses avec leur score de pertinence
    const responseScores = new Map();
    
    // Pour chaque question possible
    for (const question of gameQuestions) {
        let bestScore = 0;
        
        // Vérifier chaque phrase du message
        for (const phrase of messagePhrases) {
            for (const keyword of question.keywords) {
                // Si c'est une salutation courte, vérifier l'exactitude
                if (shortGreetings.has(keyword) && shortGreetings.has(phrase)) {
                    if (keyword === phrase) {
                        bestScore = Math.max(bestScore, 1);
                    }
                    continue;
                }
                
                // Sinon, calculer la similarité
                if (phrase.length > 1 && keyword.length > 1) {
                    const similarity = 1 - levenshteinDistance(phrase, keyword) / Math.max(phrase.length, keyword.length);
                    bestScore = Math.max(bestScore, similarity);
                }
            }
        }
        
        // Si le score est suffisant, ajouter la réponse
        if (bestScore >= 0.5) {
            responseScores.set(question.response, bestScore);
        }
    }
    
    // Si aucune réponse trouvée
    if (responseScores.size === 0) {
        return "Je ne suis pas sûr de comprendre votre question. Vous pouvez me demander des informations sur :\n" +
               "- Les règles du jeu\n" +
               "- Comment jouer\n" +
               "- Les déplacements des lutins\n" +
               "- La gestion des ponts\n" +
               "- Les conditions de victoire\n" +
               "- L'intelligence artificielle\n" +
               "- Les stratégies de jeu";
    }
    
    // Trier les réponses par score de pertinence
    const sortedResponses = Array.from(responseScores.entries())
        .sort((a, b) => b[1] - a[1])
        .map(([response]) => response);
    
    // Retourner les réponses les plus pertinentes
    return sortedResponses.join('\n\n');
}

// Fonction pour consulter le bot Prolog
async function getPrologResponse(message) {
    try {
        // Créer une nouvelle session Prolog
        const session = pl.create();

        // Charger le module lists
        await new Promise((resolve, reject) => {
            session.consult("lists", {
                success: resolve,
                error: reject
            });
        });

        // Charger notre fichier de règles Prolog
        await new Promise((resolve, reject) => {
            session.consult("pbot-elm.pl", {
                success: resolve,
                error: reject
            });
        });

        // Convertir le message en une liste de mots pour Prolog
        const words = message.toLowerCase()
            .replace(/['']/g, "'")
            .replace(/[\u00e0\u00e2]/g, "a")
            .replace(/[\u00e8\u00e9\u00ea\u00eb]/g, "e")
            .replace(/[\u00ee\u00ef]/g, "i")
            .replace(/[\u00f4]/g, "o")
            .replace(/[\u00f9\u00fb]/g, "u")
            .replace(/[\u00e7]/g, "c")
            .replace(/[^a-z0-9\s]/g, ' ')
            .trim()
            .split(/\s+/)
            .filter(word => word.length > 0);

        // Créer la requête Prolog
        const query = `produire_reponse([${words.map(w => `'${w}'`).join(',')}], Response)`;

        // Exécuter la requête
        return new Promise((resolve, reject) => {
            session.query(query, {
                success: function(goal) {
                    session.answer({
                        success: function(answer) {
                            if (answer) {
                                const response = pl.format_answer(answer);
                                // Convertir la réponse Prolog en texte lisible
                                const cleanResponse = response.Response
                                    .map(item => typeof item === 'string' ? item : item.toString())
                                    .join(' ');
                                resolve(cleanResponse);
                            } else {
                                resolve("Je ne sais pas comment répondre à cette question.");
                            }
                        },
                        error: reject,
                        fail: function() {
                            resolve("Je ne comprends pas la question.");
                        }
                    });
                },
                error: reject
            });
        });
    } catch (error) {
        console.error('Erreur lors de la communication avec Prolog:', error);
        // En cas d'erreur, utiliser la version JavaScript comme fallback
        return getBotResponse(message);
    }
}

// Initialize the bot when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    console.log('DOM loaded, initializing bot...');
    initBot();
});

// Also handle the case where the script is loaded after DOMContentLoaded
if (document.readyState === 'complete' || document.readyState === 'interactive') {
    console.log('DOM already loaded, initializing bot...');
    initBot();
}
