
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.LLM.Clients.Claude (ClaudeResponse(..), ClaudeClientConfig (..)) where

import RIO (Text, Generic, fromMaybe, ByteString)
import Web.LLM.Completion (Completion (..))
import Web.LLM.Clients.Clients (LLMClient, chatCompletion, CompletionConfig (model, maxTokens))
import Data.Aeson (ToJSON(toJSON), object, (.=), (.:), FromJSON (parseJSON), withObject)
import Web.LLM.ChatMessage ( ChatMessage(content, role), Role(..) ) 
import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      header,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      POST(POST),
      ReqBodyJson(ReqBodyJson) )

instance LLMClient ClaudeClientConfig ClaudeResponse where
  chatCompletion :: ClaudeClientConfig -> [ChatMessage] -> CompletionConfig -> IO ClaudeResponse
  chatCompletion c messages cConfig = do
    let url =
          https "api.anthropic.com"
            /: "v1"
            /: "messages"
    response <-
      runReq defaultHttpConfig $
        req
          POST
          url
          (ReqBodyJson (toClaudeRequest messages cConfig))
          jsonResponse
          (header "Content-Type" "application/json" <> header "x-api-key" (apiKey c) <> header "anthropic-version" (anthropicVersion c))
    return (responseBody response)

instance Completion ClaudeResponse where
  completion = Just . claudeResponseContentText . claudeResponseContent
  promptTokens = Just . inputTokens . usage
  completionTokens = Just . outputTokens . usage

data ClaudeClientConfig = ClaudeClientConfig { apiKey :: ByteString, anthropicVersion :: ByteString }
data ClaudeStopReason = EndTurn

instance FromJSON ClaudeStopReason where
  parseJSON = withObject "ClaudeStopReason" $ \_ -> return EndTurn

data ClaudeResponseType = Message
instance FromJSON ClaudeResponseType where
  parseJSON = withObject "ClaudeStopReason" $ \_ -> return Message
data ClaudeResponseContentType = Text deriving (FromJSON, Generic)

data ClaudeResponse = ClaudeResponse {
  claudeResponseContent :: !ClaudeResponseContent,
  id :: !Text,
  claudeResponseModel :: !Text,
  stopReason :: !ClaudeStopReason,
  stopSequence :: Maybe Text,
  responseType :: !ClaudeResponseType,
  usage :: !ClaudeUsage
}

instance FromJSON ClaudeResponse where
  parseJSON = withObject "ClaudeResponse" $ \v -> ClaudeResponse
    <$> v .: "content"
    <*> v .: "id"
    <*> v .: "model"
    <*> v .: "stop_reason"
    <*> v .: "stop_sequence"
    <*> v .: "response_type"
    <*> v .: "usage"

data ClaudeResponseContent = ClaudeResponseContent {
  claudeResponseContentText :: !Text,
  claudeResponseContentType :: !ClaudeResponseContentType
}

instance FromJSON ClaudeResponseContent where
  parseJSON = withObject "ClaudeResponseContent" $ \v -> ClaudeResponseContent
    <$> v .: "text"
    <*> v .: "type"


data ClaudeUsage = ClaudeUsage {
  inputTokens :: Int,
  outputTokens :: Int
}

instance FromJSON ClaudeUsage where
  parseJSON = withObject "ClaudeUsage" $ \v -> ClaudeUsage
    <$> v .: "input_tokens"
    <*> v .: "output_tokens"


data ClaudeRequest = ClaudeRequest {
  claudeRequestModel :: !Text,
  claudeRequestMaxTokens :: !Int,
  claudeRequestMessages :: ![ClaudeRequestBodyMessage]
}

instance ToJSON ClaudeRequest where
  toJSON c = object ["model" .= claudeRequestModel c, "max_tokens" .= claudeRequestMaxTokens c, "messages" .= claudeRequestMessages c]

data ClaudeRequestBodyMessage = ClaudeRequestBodyMessage {
  claudeRequestMessageRole :: !Role,
  claudeRequestMessageContent :: !Text
}

instance ToJSON ClaudeRequestBodyMessage where
  toJSON c = object ["role" .= parsedRole (claudeRequestMessageRole c), "content" .= claudeRequestMessageContent c]
    where
    parsedRole :: Role -> Text
    parsedRole User = "user"
    parsedRole System = "assistant"

toClaudeRequest :: [ChatMessage] -> CompletionConfig -> ClaudeRequest
toClaudeRequest messages cConfig =
  let claudeMessages = map (\r -> ClaudeRequestBodyMessage {claudeRequestMessageRole = fromMaybe User $ role r, claudeRequestMessageContent = content r}) messages
   in ClaudeRequest
        { claudeRequestModel = model cConfig,
          claudeRequestMaxTokens = maxTokens cConfig,
          claudeRequestMessages = claudeMessages
        }
