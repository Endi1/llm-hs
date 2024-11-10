module Web.LLM.ChatMessage (Role (..), ChatMessage(ChatMessage), createChatMessage) where

import RIO.Text

data Role = System | User

data ChatMessage = ChatMessage {
  role :: Maybe Role,
  content :: Text
}

createChatMessage :: Maybe Role -> Text -> ChatMessage
createChatMessage = ChatMessage
