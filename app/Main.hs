{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Char
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Data.Validation
import           Database.Persist.Postgresql
import qualified Forge.Generate as Forge
import qualified Forge.Internal.Types as Forge
import qualified Forge.Lucid as Forge
import qualified Forge.Verify as Forge
import           Lucid
import           Text.Lucius
import           Text.Read (readMaybe)
import           Types
import           Yesod hiding (Html, toHtml, textField)
import           Yesod.Lucid

-------------------------------------------------------------------------------
-- Dispatcher

data App = App (Pool SqlBackend)
instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Group
  postcode Text
  created UTCTime
Member
  group GroupId
  code Text
  created UTCTime
  status Status Maybe
  title Text
  Unique MemberCodeGroup group code
Message
  member MemberId
  group GroupId
  text Text
  created UTCTime
  joined UTCTime Maybe
|]

mkYesod "App" [parseRoutes|
  /appcss AppCssR GET
  / HomeR GET
  /create-group CreateGroupR POST GET
  /dashboard DashboardR GET
|]

--------------------------------------------------------------------------------
-- Routes

getDashboardR :: Handler (Html ())
getDashboardR = do
  (groupId, memberId) <- getSessionInfo
  Group {..} <-
    runDB
      (do grp <- get404 groupId
          pure grp)
  htmlWithUrl
    (layoutWrapper
       (div_
          [class_ "wrap"]
          (h2_
             (do "Dashboard for "
                 toHtml groupPostcode))))

getCreateGroupR :: Handler (Html ())
getCreateGroupR =
  htmlWithUrl
    (layoutWrapper
       (div_
          [class_ "wrap"]
          (startGroupHtml (Forge.view (Forge.verified createGroupForm)))))

postCreateGroupR :: Handler (Html ())
postCreateGroupR = do
  (inputs, _files) <- runRequestBody
  let inputMap =
        (M.fromListWith
           (<>)
           (map (first Forge.Key . second (pure . Forge.TextInput)) inputs))
  case runIdentity (Forge.generate inputMap (Forge.verified createGroupForm)) of
    Forge.Generated {generatedView = html, generatedValue = v} ->
      case v of
        Failure _errs ->
          htmlWithUrl
            (layoutWrapper (div_ [class_ "wrap"] (startGroupHtml html)))
        Success postCode -> do
          memberCode <- liftIO generateCode
          now <- liftIO getCurrentTime
          (groupId, memberId) <-
            runDB
              (do gid <-
                    insert Group {groupPostcode = postCode, groupCreated = now}
                  memberId <- insert
                    Member
                      { memberGroup = gid
                      , memberCode
                      , memberCreated = now
                      , memberStatus = Nothing
                      , memberTitle = ""
                      }
                  pure (gid, memberId))
          setSession "groupId" (T.pack (show (fromSqlKey groupId)))
          setSession "memberId" (T.pack (show (fromSqlKey memberId)))
          redirect DashboardR

getHomeR :: Handler (Html ())
getHomeR = do
  htmlWithUrl
    (layoutWrapper
       (do div_
             [class_ "wrap"]
             (div_
                [class_ "home-choice"]
                (do div_
                      [class_ "choice"]
                      (do h2_ "I have a code"
                          p_
                            "If someone has created a group for your street or apartment block, \
                                         \you should have received a code in your letterbox or in person."
                          url <- ask
                          form_
                            [action_ (url HomeR), method_ "POST"]
                            (do p_
                                  (do label_ "Postcode: "
                                      input_ [type_ "text", name_ "postcode"])
                                p_
                                  (do label_ "Member Code: "
                                      input_ [type_ "text", name_ "code"])
                                p_ (button_ "LOGIN")))
                    div_
                      [class_ "choice"]
                      (startGroupHtml
                         (Forge.view (Forge.verified createGroupForm)))))
           div_
             [class_ "wrap"]
             (do h3_ "Information"
                 p_
                   (do "UK Government information: "
                       a_
                         [ href_
                             "https://www.gov.uk/government/topical-events/coronavirus-covid-19-uk-government-response"
                         ]
                         "Coronavirus (COVID-19): what you need to do")
                 p_
                   "This is a voluntarily run service to aid small communities (streets and tower blocks) to self-report, coordinate, and ask for help, during the self-isolation stage of this pandemic."
                 p_ (strong_ "Isolating.org is NOT a government-run web site.")
                 h3_ "Why are codes needed?"
                 p_
                   "To provide privacy for communities. You can only get a code if a group creator \
                                \as provided you with one, via your letterbox or handed to you in person. This \
                               \protects against abuse.")))

--------------------------------------------------------------------------------
-- Page wrapper

layoutWrapper ::
  MonadReader (Route App -> Text) m => HtmlT m b -> HtmlT m b
layoutWrapper inner = do
  doctype_
  url <- ask
  html_
    (do head_
          (do link_ [rel_ "shortcut icon", href_ "#"]
              title_ "Self Isolating with Coronavirus (COVID-19)"
              link_ [rel_ "stylesheet", type_ "text/css", href_ (url AppCssR)])
        body_
          (do div_
                [class_ "heading"]
                (div_
                   [class_ "wrap"]
                   (h1_ (a_ [href_ (url HomeR)] "Self Isolating with Coronavirus (COVID-19)")))
              inner))

--------------------------------------------------------------------------------
-- Start a group

startGroupHtml :: HtmlT Identity () -> Lucid App ()
startGroupHtml formHtml = do
  h2_ "Start a group for your street or block of apartments"
  p_
    "Before starting a group, please confirm that there is not \
                       \already a group created for your street or apartment block."
  url <- ask
  form_
    [action_ (url CreateGroupR), method_ "POST"]
    (do relaxHtmlT formHtml
        p_ (button_ "GET CODE"))

--------------------------------------------------------------------------------
-- Forms

createGroupForm :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
createGroupForm =
  wrapErrorsAllowBubble
    "Postcode"
    (wrapHtml
       (\html -> p_
                   (do label_ "Postcode: "
                       html))
       postcodeField)

--------------------------------------------------------------------------------
-- Fields

postcodeField :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
postcodeField =
  Forge.ParseForm
    (\text ->
       if T.all (\c -> isAlphaNum c) (T.filter (not . isSpace) text)
         then pure (Right (T.strip text))
         else pure (Left InvalidPostCode))
    requiredTextField

--------------------------------------------------------------------------------
-- Forge utilities

data IsolatingError
  = LucidError Forge.Error
  | TextNotProvided
  | InvalidPostCode
  | ContextedError Text
                   IsolatingError
  deriving (Show)

-- | Limited to the sum type 'Error' defined in this module.
instance Forge.FormError IsolatingError where
  missingInputError = LucidError . Forge.MissingInput
  invalidInputFormat x = LucidError . Forge.InvalidInputFormat x

-- | Wrap a formlet with its errors. Allow errors to bubble up.
wrapErrorsAllowBubble ::
     Text
  -> Forge.Form index Identity (Html ()) Forge.Field IsolatingError a
  -> Forge.Form index Identity (Html ()) Forge.Field IsolatingError a
wrapErrorsAllowBubble label =
  Forge.CeilingForm
    (\errors html ->
       ( do html
            unless
              (null errors)
              (ul_ [class_ "inline-errors"] (mapM_ (li_ . showError) errors))
       , map (ContextedError label) errors))

wrapHtml ::
  (t -> t)
  -> Forge.Form index parse t field error a
  -> Forge.Form index parse t field error a
wrapHtml f = Forge.CeilingForm (\es v -> (f v, es))

-- | Convert an error to HTML.
showError :: Monad m => IsolatingError -> HtmlT m ()
showError =
  \case
    InvalidPostCode -> "Invalid postal or zip code."
    TextNotProvided -> "No text was provided."
    ContextedError label e -> do
      toHtml label
      ": "
      showError e
    LucidError err ->
      case err of
        Forge.MissingInput {} -> "Missing input: try resubmitting the form?"
        Forge.InvalidInputFormat {} -> "Invalid input format."

requiredTextField :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
requiredTextField =
  Forge.ParseForm
    (\t ->
       pure
         (if T.null t
            then Left TextNotProvided
            else Right t))
    (Forge.FieldForm Forge.DynamicFieldName (Forge.TextField Nothing))

dropdownField ::
     Eq a
  => Maybe a
  -> NonEmpty (a, Text)
  -> Forge.Form index parse (Html ()) Forge.Field IsolatingError a
dropdownField mdef options =
  Forge.FieldForm Forge.DynamicFieldName (Forge.DropdownField mdef options)

--------------------------------------------------------------------------------
-- Code generation

-- | Generate a 12-character code.
generateCode :: IO Text
generateCode = do
  uuid <- UUID.nextRandom
  pure (T.take 12 (T.reverse (UUID.toText uuid)))

--------------------------------------------------------------------------------
-- Static

getAppCssR :: Handler Css
getAppCssR = pure ($(luciusFile "templates/app.lucius") ())

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main =
  runNoLoggingT
    (withPostgresqlPool
       "dbname=isolating user=isolating password=isolating host=localhost"
       10
       (\pool -> do runSqlPool (runMigration migrateAll) pool
                    liftIO (warpEnv (App pool))))

--------------------------------------------------------------------------------
-- Session

getSessionInfo :: Handler (GroupId, MemberId)
getSessionInfo = do
  gid <- lookupSession "groupId"
  mid <- lookupSession "memberId"
  case (,) <$> (gid >>= (readMaybe . T.unpack)) <*> (mid >>= (readMaybe . T.unpack)) of
    Just (g, m) -> pure (toSqlKey g, toSqlKey m)
    Nothing -> redirect HomeR
