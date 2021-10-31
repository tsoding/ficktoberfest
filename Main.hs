{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.Vector as V
import Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types.Status (Status(..))
import System.Environment
import Text.Printf

newtype GithubToken =
  GithubToken T.Text

newtype Owner =
  Owner T.Text
  deriving (Show)

instance FromJSON Owner where
  parseJSON (Object v) = Owner <$> v .: "login"
  parseJSON invalid = typeMismatch "Owner" invalid

data Repo = Repo
  { repoName :: !T.Text
  , repoOwner :: !Owner
  } deriving (Show)

instance FromJSON Repo where
  parseJSON (Object v) = Repo <$> v .: "name" <*> v .: "owner"
  parseJSON invalid = typeMismatch "Repo" invalid

newtype Base = Base
  { baseRepo :: Repo
  } deriving (Show)

instance FromJSON Base where
  parseJSON (Object v) = Base <$> v .: "repo"
  parseJSON invalid = typeMismatch "Base" invalid

data PullRequestEvent = PullRequestEvent
  { pullRequestEventId :: !T.Text
  , pullRequestEventPull :: !Pull
  } deriving (Show)

instance FromJSON PullRequestEvent where
  parseJSON (Object v) =
    PullRequestEvent <$> v .: "id" <*> (v .: "payload" >>= (.: "pull_request"))
  parseJSON invalid = typeMismatch "PullRequestEvent" invalid

data Event = Event
  { eventType :: !T.Text
  , eventId :: !T.Text
  } deriving (Show)

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "type" <*> v .: "id"
  parseJSON invalid = typeMismatch "Event" invalid

data Pull = Pull
  { pullUrl :: !T.Text
  , pullNumber :: !Int
  , pullTitle :: !T.Text
  , pullCreatedAt :: !UTCTime
  , pullState :: !T.Text
  , pullBase :: !Base
  } deriving (Show)

instance FromJSON Pull where
  parseJSON (Object v) =
    Pull <$> v .: "url" <*> v .: "number" <*> v .: "title" <*> v .: "created_at" <*>
    v .: "state" <*>
    v .: "base"
  parseJSON invalid = typeMismatch "Pull" invalid

data Github = Github
  { githubToken :: !GithubToken
  , githubManager :: !Manager
  }

newGithub :: FilePath -> IO Github
newGithub tokenFile = do
  manager <- TLS.newTlsManager
  token <- GithubToken . T.strip <$> T.readFile tokenFile
  return $ Github token manager

newtype ETag =
  ETag B.ByteString
  deriving (Show)

data Poller a = Poller
  { pollerETag :: !(Maybe ETag)
  , pollerInterval :: !(Maybe Int)
  , pollerPayload :: !a
  } deriving (Show)

pollEvents :: Github -> Owner -> Maybe ETag -> IO (Poller [PullRequestEvent])
pollEvents Github {githubToken = GithubToken token, githubManager = manager} (Owner owner) etag = do
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
        fmap (read . map (chr . fromIntegral) . B.unpack) $
        lookup "X-Poll-Interval" $ responseHeaders response
  case responseStatus response of
    Status 304 _ -> return $ Poller etag' interval []
    Status 200 _ -> do
      events <- either error return $ eitherDecode $ responseBody response
      let pullRequestEvents =
            mapMaybe
              (\case
                 v'@(Object v)
                   | HM.lookup "type" v == Just (String "PullRequestEvent") ->
                     case fromJSON v' of
                       Success x -> Just x
                       Error _ -> error "Failed to parse"
                 _ -> Nothing)
              events
      return $ Poller etag' interval pullRequestEvents
    Status x _ -> error $ printf "Got unexpected status %d" x

invalidatePull :: Github -> Pull -> IO ()
-- WTF IS THIS LINE HINDENT?!!
invalidatePull Github {githubManager = manager, githubToken = GithubToken token} Pull { pullBase = Base {baseRepo = Repo { repoName = repo
                                                                                                                         , repoOwner = Owner owner
                                                                                                                         }}
                                                                                      , pullNumber = number
                                                                                      } = do
  let url =
        printf
          "https://api.github.com/repos/%s/%s/issues/%d/labels"
          owner
          repo
          number
  let payload = object ["labels" .= Array (V.fromList ["invalid"])]
  request <- parseRequest url
  response <-
    httpLbs
      (request
         { method = "POST"
         , requestHeaders =
             ("User-Agent", T.encodeUtf8 "Ficktoberfest") :
             ("Authorization", T.encodeUtf8 $ "token " <> token) :
             requestHeaders request
         , requestBody = RequestBodyLBS $ encode payload
         })
      manager
  print response
  return ()

pollLoop :: Github -> Owner -> Maybe T.Text -> Maybe ETag -> IO ()
pollLoop github owner lastEventId etag = do
  Poller etag' interval events <- pollEvents github owner etag
  let unprocessedEvents =
        filter (\x -> lastEventId < Just (pullRequestEventId x)) events
  mapM_ (invalidatePull github . pullRequestEventPull) unprocessedEvents
  maybe (return ()) (threadDelay . (* 1000000)) interval
  pollLoop
    github
    owner
    ((pullRequestEventId <$> listToMaybe unprocessedEvents) <|> lastEventId)
    etag'

githubRequest :: Github -> Request -> IO (Response Value)
githubRequest Github {githubManager = manager, githubToken = GithubToken token} request = do
  response <-
    httpLbs
      (request
         { requestHeaders =
             ("User-Agent", T.encodeUtf8 "Ficktoberfest") :
             ("Authorization", T.encodeUtf8 $ "token " <> token) :
             requestHeaders request
         })
      manager
  return $ either error id . eitherDecode <$> response

resultToEither :: Result a -> Either String a
resultToEither (Success x) = Right x
resultToEither (Error s) = Left s

data Args = Args { argTokenFile :: String, argOwner :: Owner }

parseArgs :: [String] -> Args
parseArgs [] = error "Usage: ficktoberfest <tokenFile> [owner]"
parseArgs [tokenFile'] = Args tokenFile' (Owner "tsoding")
parseArgs (tokenFile:owner:_) = Args tokenFile (Owner $ T.pack owner)

mainWithArgs :: Args -> IO ()
mainWithArgs args = do
  github <- newGithub $ argTokenFile args
  putStrLn "Waiting for Pull Requests..."
  pollLoop github (argOwner args) Nothing Nothing

main :: IO ()
main = getArgs >>= mainWithArgs . parseArgs
