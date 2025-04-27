{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.LLM.Clients.Gemini (GeminiResponse, GeminiClientConfig (..), chatCompletion) where

import RIO (Text, (^.), Generic)
import RIO.Lens (each)
import Web.LLM.Completion (Completion (..))
import Web.LLM.Clients.Clients (LLMClient, chatCompletion, CompletionConfig (model))
import Data.Aeson (ToJSON(toJSON), object, (.=), FromJSON)
import Web.LLM.ChatMessage (ChatMessage(ChatMessage))
import Network.HTTP.Req
import RIO.Text (append)

newtype GeminiClientConfig = GeminiClientConfig { apiKey :: Text }
  
instance LLMClient GeminiClientConfig GeminiResponse where
  chatCompletion :: GeminiClientConfig -> [ChatMessage] -> CompletionConfig -> IO GeminiResponse
  chatCompletion c messages cConfig = do
    let url = https "generativelanguage.googleapis.com"
              /: "v1beta"
              /: "models"
              /: (model cConfig `append` ":generateContent")
    response <- runReq defaultHttpConfig $ req POST url
      (ReqBodyJson (toGeminiRequest messages cConfig))
      jsonResponse
      (queryParam "key" (Just $ apiKey c) <> header "Content-Type" "application/json")
    return (responseBody response)

toGeminiRequest :: [ChatMessage] -> CompletionConfig -> GeminiRequest
toGeminiRequest messages _ = GeminiRequest [
  GeminiRequestContent $ map (\(ChatMessage _ c) -> GeminiRequestPart c) messages
  ]

data GeminiResponse = GeminiResponse
  { candidates :: ![GeminiResponseCandidate],
    usageMetadata :: !GeminiResponseUsageMetadata
  } deriving (Generic, FromJSON, Show)

data GeminiResponseUsageMetadata = GeminiResponseUsageMetadata
  { promptTokenCount :: !Int,
    candidatesTokenCount :: !Int,
    totalTokenCount :: !Int
  } deriving (Generic, FromJSON, Show)

newtype GeminiResponseCandidate = GeminiResponseCandidate
  { content :: GeminiResponseCandidateContent
  } deriving (Generic, FromJSON, Show)

newtype GeminiResponseCandidateContent = GeminiResponseCandidateContent
  { parts :: [GeminiResponseCandidateContentPart]
  } deriving (Generic, FromJSON, Show)

newtype GeminiResponseCandidateContentPart = GeminiResponseCandidateContentPart
  { text :: Text
  } deriving (Generic, FromJSON, Show)


newtype GeminiRequest = GeminiRequest {grContents :: [GeminiRequestContent]}
newtype GeminiRequestContent = GeminiRequestContent { grcParts :: [GeminiRequestPart] }
newtype GeminiRequestPart = GeminiRequestPart { grpText :: Text}

instance ToJSON GeminiRequest where
  toJSON r = object [ "contents" .= grContents r ]

instance ToJSON GeminiRequestContent where
  toJSON c = object [ "parts" .= grcParts c ]

instance ToJSON GeminiRequestPart where
  toJSON p = object
    [
      "text" .= grpText p
    ]

instance Completion GeminiResponse where
  completion geminiResponse = case candidates geminiResponse of
    [] -> Nothing
    (candidate:_) -> Just $ map text (parts $ content candidate) ^. each
  promptTokens geminiResponse = Just $ promptTokenCount $ usageMetadata geminiResponse
  completionTokens geminiResponse = Just $ candidatesTokenCount $ usageMetadata geminiResponse
