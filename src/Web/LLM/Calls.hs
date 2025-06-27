module Web.LLM.Calls (geminiChatCompletion, claudeChatCompletion) where

import Web.LLM.Clients.Clients (chatCompletion)
import Web.LLM.Clients.Gemini (GeminiClientConfig, GeminiResponse)
import Web.LLM.ChatMessage (ChatMessage)
import Web.LLM.Clients.Clients (CompletionConfig)
import Web.LLM.Clients.Claude (ClaudeClientConfig(..), ClaudeResponse)

geminiChatCompletion :: GeminiClientConfig -> [ChatMessage] -> CompletionConfig -> IO GeminiResponse
geminiChatCompletion = chatCompletion

claudeChatCompletion :: ClaudeClientConfig -> [ChatMessage] -> CompletionConfig -> IO ClaudeResponse
claudeChatCompletion = chatCompletion
