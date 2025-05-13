/**
 * PontuXL Bot Interface
 * This file handles the communication between the web interface and the Prolog bot
 */

// DOM elements
const chatMessages = document.getElementById('chat-messages');
const userInput = document.getElementById('user-input');
const sendButton = document.getElementById('send-button');

// Sample responses for the bot (will be replaced with Prolog integration)
const sampleResponses = {
    'qui commence': "Par convention, c'est au joueur en charge des lutins verts de commencer la partie.",
    'combien de lutins': "Chaque équipe compte 4 lutins.",
    'comment gagner': "Le dernier joueur non éliminé gagne la partie. Un joueur est éliminé lorsque tous ses lutins se retrouvent sans pont autour d'eux.",
    'comment jouer': "À votre tour, vous devez déplacer un de vos lutins d'une case à une autre en empruntant un pont, puis retirer un pont ou tourner un pont d'un quart de tour.",
    'règles': "Pontu est un jeu où vous déplacez vos lutins et retirez des ponts pour isoler les lutins adverses. Un joueur est éliminé quand tous ses lutins sont isolés.",
    'déplacer lutin': "Vous pouvez déplacer un lutin vers une case adjacente en passant par un pont. Deux lutins ne peuvent pas occuper la même case.",
    'retirer pont': "Après avoir déplacé un lutin, vous devez retirer un pont. Vous pouvez retirer le pont que vous venez d'emprunter ou n'importe quel autre pont.",
    'tourner pont': "Au lieu de retirer un pont, vous pouvez choisir de tourner un pont d'un quart de tour.",
    'taille plateau': "Dans PontuXL, le plateau est de taille 6x6.",
    'ordre joueurs': "Les joueurs jouent dans l'ordre suivant : vert, bleu, jaune, rouge.",
    'intelligence artificielle': "Dans cette version du jeu, les lutins bleus et rouges sont contrôlés par une intelligence artificielle."
};

// Initialize the bot
function initBot() {
    // Add event listeners
    sendButton.addEventListener('click', handleUserMessage);
    userInput.addEventListener('keypress', (event) => {
        if (event.key === 'Enter') {
            handleUserMessage();
        }
    });
}

// Handle user message
function handleUserMessage() {
    const message = userInput.value.trim();
    
    if (message === '') return;
    
    // Display user message
    addMessage(message, 'user');
    
    // Clear input field
    userInput.value = '';
    
    // Process the message and get bot response
    setTimeout(() => {
        const response = getBotResponse(message);
        addMessage(response, 'bot');
    }, 500);
}

// Add a message to the chat
function addMessage(message, sender) {
    const messageElement = document.createElement('div');
    messageElement.className = sender === 'user' ? 'user-message' : 'bot-message';
    messageElement.textContent = message;
    
    chatMessages.appendChild(messageElement);
    
    // Scroll to bottom
    chatMessages.scrollTop = chatMessages.scrollHeight;
}

// Get bot response based on user input
function getBotResponse(message) {
    // Convert message to lowercase for easier matching
    const lowerMessage = message.toLowerCase();
    
    // Check if message contains any keywords
    for (const [keyword, response] of Object.entries(sampleResponses)) {
        if (lowerMessage.includes(keyword)) {
            return response;
        }
    }
    
    // Check for specific questions
    if (lowerMessage.includes('fin') || lowerMessage.includes('au revoir') || lowerMessage.includes('merci')) {
        return "Merci de m'avoir consulté. À bientôt!";
    }
    
    // Default response
    return "Je ne sais pas répondre à cette question. Essayez de me demander à propos des règles du jeu, comment déplacer les lutins, ou comment retirer des ponts.";
}

// Future function to integrate with Prolog backend
async function getPrologResponse(message) {
    try {
        // This would be replaced with actual Prolog integration
        // For example, using WebSockets or HTTP requests to a Prolog server
        
        // Placeholder for now
        return getBotResponse(message);
    } catch (error) {
        console.error('Error communicating with Prolog backend:', error);
        return "Désolé, j'ai rencontré une erreur en essayant de traiter votre demande.";
    }
}

// Initialize the bot when the page loads
window.addEventListener('DOMContentLoaded', initBot);
