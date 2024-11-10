module Web.LLM.Completion (Completion (..)) where
import RIO (Text)

class Completion a where
  completion :: a -> Maybe Text
  promptTokens :: a -> Maybe Int
  completionTokens :: a -> Maybe Int
