module Web.LLM.Calls (geminiChatCompletion) where

import Web.LLM.Clients.Gemini (chatCompletion, GeminiClientConfig, GeminiResponse)
import Web.LLM.ChatMessage (ChatMessage)
import Web.LLM.Clients.Clients (CompletionConfig)


geminiChatCompletion :: GeminiClientConfig -> [ChatMessage] -> CompletionConfig -> IO GeminiResponse
geminiChatCompletion = chatCompletion
