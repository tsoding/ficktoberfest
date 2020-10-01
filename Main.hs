{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Client
import Network.HTTP.Types.Status (Status(statusCode))
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Aeson
import Network.HTTP.Types.Status
import Text.Printf
import Control.Monad
import Data.Maybe
import qualified Data.ByteString as B
import Data.Char
import Control.Concurrent
import Control.Applicative
import qualified Data.HashMap.Strict as HM

newtype GithubToken = GithubToken T.Text
newtype Owner = Owner T.Text
newtype Repo = Repo T.Text

data PullRequestEvent = PullRequestEvent
  { pullRequestEventId :: T.Text
  , pullRequestEventPull :: Pull
  } deriving (Show)

instance FromJSON PullRequestEvent where
  parseJSON (Object v) = PullRequestEvent <$> v .: "id" <*> (v .: "payload" >>= (.: "pull_request"))

data Event = Event
  { eventType :: T.Text
  , eventId :: T.Text
  } deriving (Show)

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "type" <*> v .: "id"

data Pull = Pull
  { pullUrl :: T.Text
  , pullNumber :: Int
  , pullTitle :: T.Text
  , pullCreatedAt :: UTCTime
  , pullState :: T.Text
  } deriving (Show)

instance FromJSON Pull where
  parseJSON (Object v) =
    Pull <$> v .: "url" <*> v .: "number" <*> v .: "title" <*> v .: "created_at" <*>
    v .: "state"

data Github = Github
  { githubToken :: GithubToken
  , githubManager :: Manager
  }

newGithub :: FilePath -> IO Github
newGithub tokenFile = do
  manager <- TLS.newTlsManager
  token <- GithubToken . T.strip <$> T.readFile tokenFile
  return $ Github token manager

newtype ETag =
  ETag B.ByteString
  deriving (Show)

pollEvents :: Github -> Owner -> Maybe ETag -> IO (Maybe ETag, Maybe Int, [PullRequestEvent])
pollEvents github@Github { githubToken = GithubToken token
                         , githubManager = manager
                         } (Owner owner) etag = do
  let url = printf "https://api.github.com/users/%s/events" owner
  let authHeaders =
        [ ("User-Agent", T.encodeUtf8 "Ficktoberfest")
        , ("Authorization", T.encodeUtf8 $ "token " <> token)
        ] ++
        maybeToList (fmap (\(ETag x) -> ("If-None-Match", x)) etag)
  request <- parseRequest url
  response <-
    httpLbs
      (request {requestHeaders = authHeaders ++ requestHeaders request})
      manager
  let etag' = fmap ETag $ lookup "ETag" $ responseHeaders response
  let interval =
        fmap (\x -> read $ map (chr . fromIntegral) $ B.unpack x) $
        lookup "X-Poll-Interval" $ responseHeaders response
  case responseStatus response of
    Status 304 _ -> return (etag', interval, [])
    Status 200 _ -> do
      events <- either error return $ eitherDecode $ responseBody response
      let pullRequestEvents =
            mapMaybe
              (\case
                 v'@(Object v)
                   | (HM.lookup "type" v) == Just (String "PullRequestEvent") ->
                     case fromJSON v' of
                       Success x -> Just x
                       Error _ -> error "Failed to parse"
                 _ -> Nothing)
              events
      return (etag', interval, pullRequestEvents)
    Status x _ -> error $ printf "Got unexpected status %d" x

pollLoop :: Github -> Owner -> Maybe T.Text -> Maybe ETag -> IO ()
pollLoop github owner lastEventId etag = do
  (etag', interval, events) <- pollEvents github owner etag
  let unprocessedEvents =
        filter (\x -> lastEventId < Just (pullRequestEventId x)) events
  mapM print unprocessedEvents
  maybe (return ()) (threadDelay . (* 1000000)) interval
  pollLoop
    github
    owner
    ((pullRequestEventId <$> listToMaybe unprocessedEvents) <|> lastEventId)
    etag'

getPullRequests :: Github -> Owner -> Repo -> Int -> IO [Pull]
getPullRequests github@Github { githubToken = GithubToken token
                              , githubManager = manager
                              } owner'@(Owner owner) repo'@(Repo repo) page = do
  let url =
        "https://api.github.com/repos/" <> owner <> "/" <> repo <>
        "/pulls?state=closed&page=" <>
        (T.pack $ show page)
  request <- parseRequest $ T.unpack url
  response <-
    httpLbs
      (request
         { requestHeaders =
             ("User-Agent", T.encodeUtf8 "Ficktoberfest") :
             ("Authorization", T.encodeUtf8 $ "token " <> token) :
             requestHeaders request
         })
      manager
  let status = responseStatus response
  when (status /= status200) $ error $ show status
  prs <- either error return $ eitherDecode (responseBody response)
  if length prs > 0
    then do
      nextPrs <- getPullRequests github owner' repo' (page + 1)
      return $ prs ++ nextPrs
    else return []

mainWithArgs :: [String] -> IO ()
mainWithArgs (tokenFile:_) = do
  github <- newGithub tokenFile
  putStrLn "Waiting for Pull Requests..."
  pollLoop github (Owner "rexim") Nothing Nothing
mainWithArgs _ = error "Usage: ficktoberfest <tokenFile>"

main :: IO ()
main = getArgs >>= mainWithArgs
