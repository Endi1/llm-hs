{-# LANGUAGE MultiParamTypeClasses #-}
module Web.LLM.Clients.Clients (LLMClient (..), CompletionConfig (..), ResponseFormat (..)) where
import Web.LLM.ChatMessage (ChatMessage)
import RIO (Text)
import Web.LLM.Completion (Completion)

type Model = Text
data ResponseFormat = JSON | PlainText

data CompletionConfig = CompletionConfig
  { model :: Model,
    maxTokens :: Int,
    timeout :: Maybe Int,
    temperature :: Float,
    responseFormat :: ResponseFormat
  }

class Completion r => LLMClient c r where
  chatCompletion :: c -> [ChatMessage] -> CompletionConfig -> IO r
