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
import           Data.List (sortBy)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord
import           Data.Pool
import           Data.Set (Set)
import qualified Data.Set as Set
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
  joined UTCTime Maybe
  password Text Maybe
  condition Condition Maybe
  title Text
  updated UTCTime
  origin MemberId Maybe
  Unique MemberCodeGroup group code
Message
  member MemberId
  group GroupId
  condition Condition
  title Text
  desc Text
  created UTCTime
|]

mkYesod "App" [parseRoutes|
  /appcss AppCssR GET
  / HomeR GET
  /create-group CreateGroupR POST GET
  /dashboard DashboardR GET
  /post PostR GET POST
  /password PassR GET POST
  /login LoginR GET POST
  /unlock UnlockR GET POST
  /generate GenerateR GET POST
  /logout LogoutR POST
|]

--------------------------------------------------------------------------------
-- Routes

maxMembers :: Int
maxMembers = 100

postGenerateR :: Handler (Html ())
postGenerateR = do
  (groupId, memberId) <- getSessionInfo
  membersTotal <- runDB (count [MemberGroup ==. groupId])
  if membersTotal > maxMembers
    then redirect GenerateR
    else do
      now <- liftIO getCurrentTime
      runDB
        (let loop 0 = pure ()
             loop i = do
               memberCode <- liftIO generateCode
               result <-
                 insertBy
                   Member
                     { memberGroup = groupId
                     , memberCode
                     , memberCreated = now
                     , memberCondition = Nothing
                     , memberTitle = ""
                     , memberJoined = Nothing
                     , memberUpdated = now
                     , memberPassword = Nothing
                     , memberOrigin = pure memberId
                     }
               case result of
                 Left {} -> loop i
                 Right {} -> loop (i - 1)
          in loop (10 :: Int))
      redirect GenerateR

getGenerateR :: Handler (Html ())
getGenerateR = do
  (groupId, memberId) <- getSessionInfo
  members <-
    runDB
      (selectList
         [ MemberOrigin ==. pure memberId
         , MemberJoined ==. Nothing
         , MemberGroup ==. groupId
         ]
         [Asc MemberCreated])
  membersTotal <- runDB (count [MemberGroup ==. groupId])
  Group {..} <-
    runDB
      (do grp <- get404 groupId
          pure grp)
  htmlWithUrl
    (layoutWrapper
       (do div_
             [class_ "wrap"]
             (do h2_ "Invite Codes"
                 url <- ask
                 if membersTotal > maxMembers
                    then p_ (do "Maximum members reached for this group: "
                                toHtml (show membersTotal))
                    else form_
                           [action_ (url GenerateR), method_ "POST"]
                           (p_ (button_ "generate 10 codes"))
                 if null members
                   then p_ "None yet. Hit the GENERATE CODES button."
                   else do
                     p_
                       (do "Total generated by you: "
                           toHtml (show (length members)))
                     forM_
                       members
                       (\(Entity _ Member {..}) ->
                          div_
                            [class_ "invite-code"]
                            (do p_ (strong_ "isolating.org")
                                p_ (code_ (toHtml groupPostcode))
                                p_
                                  ("Coronavirus (COVID-19): Anonymously report your own and track your \
                                          \building or street's conditions.")
                                p_ ("Your code: " <> code_ (toHtml memberCode)))))))

postLogoutR :: Handler (Html ())
postLogoutR = do
  clearSession
  redirect HomeR

getLoginR :: Handler (Html ())
getLoginR = do
  htmlWithUrl
    (layoutWrapper
       (div_
          [class_ "wrap"]
          (do h2_ "Login"
              url <- ask
              form_
                [action_ (url LoginR), method_ "POST"]
                (do relaxHtmlT (Forge.view (Forge.verified loginForm))
                    p_ (button_ "LOGIN")))))

postLoginR :: Handler (Html ())
postLoginR = do
  (inputs, _files) <- runRequestBody
  let inputMap =
        (M.fromListWith
           (<>)
           (map (first Forge.Key . second (pure . Forge.TextInput)) inputs))
  case runIdentity (Forge.generate inputMap (Forge.verified loginForm)) of
    Forge.Generated {generatedView = html, generatedValue = v} ->
      case v of
        Failure _errs ->
          htmlWithUrl
            (layoutWrapper
               (div_
                  [class_ "wrap"]
                  (do h2_ "Login"
                      url <- ask
                      form_
                        [action_ (url LoginR), method_ "POST"]
                        (do relaxHtmlT html
                            p_ (button_ "LOGIN")))))
        Success (postcode, code) -> do
          mmember <-
            runDB
              (do groups <- selectList [GroupPostcode ==. postcode] []
                  selectFirst
                    [ MemberGroup <-. [groupId | Entity groupId _ <- groups]
                    , MemberCode ==. code
                    ]
                    [])
          case mmember of
            Nothing ->
              htmlWithUrl
                (layoutWrapper
                   (div_
                      [class_ "wrap"]
                      (do h2_ "Login"
                          url <- ask
                          form_
                            [action_ (url LoginR), method_ "POST"]
                            (do ul_
                                  [class_ "inline-errors"]
                                  (li_ "Incorrect details.")
                                relaxHtmlT html
                                p_ (button_ "LOGIN")))))
            Just (Entity memberId Member {..}) -> do
              setSession "nearly_groupId" (T.pack (show (fromSqlKey memberGroup)))
              setSession "nearly_memberId" (T.pack (show (fromSqlKey memberId)))
              case memberPassword of
                Nothing -> redirect PassR
                Just {} -> redirect UnlockR

getPassR :: Handler (Html ())
getPassR = do
  (groupId, _memberId) <- getNearlySessionInfo
  Group {..} <-
    runDB
      (do grp <- get404 groupId
          pure grp)
  htmlWithUrl
    (layoutWrapper
       (div_
          [class_ "wrap"]
          (do h2_ "Set password"
              p_ "Set the password for your personal use."
              p_ "Write down your password somewhere so that you don't forget it."
              url <- ask
              form_
                [action_ (url PassR), method_ "POST"]
                (do relaxHtmlT (Forge.view (Forge.verified savePassForm))
                    p_ (button_ "SAVE")))))

postPassR :: Handler (Html ())
postPassR = do
  (groupId, memberId) <- getNearlySessionInfo
  (inputs, _files) <- runRequestBody
  let inputMap =
        (M.fromListWith
           (<>)
           (map (first Forge.Key . second (pure . Forge.TextInput)) inputs))
  case runIdentity (Forge.generate inputMap (Forge.verified savePassForm)) of
    Forge.Generated {generatedView = html, generatedValue = v} ->
      case v of
        Failure _errs ->
          htmlWithUrl
            (layoutWrapper
               (div_
                  [class_ "wrap"]
                  (do h2_ "Set password"
                      url <- ask
                      form_
                        [action_ (url PassR), method_ "POST"]
                        (do relaxHtmlT html
                            p_ (button_ "SAVE")))))
        Success pass -> do
          now <- liftIO getCurrentTime
          runDB
            (do update
                  memberId
                  [ MemberJoined =. pure now
                  , MemberPassword =. pure pass
                  ])
          setSession "groupId" (T.pack (show (fromSqlKey groupId)))
          setSession "memberId" (T.pack (show (fromSqlKey memberId)))
          redirect DashboardR

getUnlockR :: Handler (Html ())
getUnlockR = do
  (groupId, _memberId) <- getNearlySessionInfo
  Group {..} <-
    runDB
      (do grp <- get404 groupId
          pure grp)
  htmlWithUrl
    (layoutWrapper
       (div_
          [class_ "wrap"]
          (do h2_ "Unlock your account"
              p_ "Your account has been protected by a password to keep your details safe."
              p_ "Please enter it now."
              url <- ask
              form_
                [action_ (url UnlockR), method_ "POST"]
                (do relaxHtmlT (Forge.view (Forge.verified enterPassForm))
                    p_ (button_ "SAVE")))))

postUnlockR :: Handler (Html ())
postUnlockR = do
  (groupId, memberId) <- getNearlySessionInfo
  (Group {..}, Member {..}) <-
    runDB
      (do grp <- get404 groupId
          mem <- get404 memberId
          pure (grp, mem))
  (inputs, _files) <- runRequestBody
  let inputMap =
        (M.fromListWith
           (<>)
           (map (first Forge.Key . second (pure . Forge.TextInput)) inputs))
  case runIdentity (Forge.generate inputMap (Forge.verified enterPassForm)) of
    Forge.Generated {generatedView = html, generatedValue = v} ->
      case v of
        Failure _errs ->
          htmlWithUrl
            (layoutWrapper
               (div_
                  [class_ "wrap"]
                  (do h2_ "Unlock "
                      url <- ask
                      form_
                        [action_ (url PassR), method_ "POST"]
                        (do relaxHtmlT html
                            p_ (button_ "SAVE")))))
        Success pass -> do
          if pure pass == memberPassword
            then do
              setSession "groupId" (T.pack (show (fromSqlKey groupId)))
              setSession "memberId" (T.pack (show (fromSqlKey memberId)))
              redirect DashboardR
            else htmlWithUrl
                   (layoutWrapper
                      (div_
                         [class_ "wrap"]
                         (do h2_ "Unlock "
                             url <- ask
                             form_
                               [action_ (url PassR), method_ "POST"]
                               (do ul_
                                     [class_ "inline-errors"]
                                     (li_ "Bad password.")
                                   relaxHtmlT html
                                   p_ (button_ "SAVE")))))

getPostR :: Handler (Html ())
getPostR = do
  (groupId, memberId) <- getSessionInfo
  (Group {..}, Member {..}) <-
    runDB
      (do grp <- get404 groupId
          mem <- get404 memberId
          pure (grp, mem))
  htmlWithUrl
    (layoutWrapper
       (div_
          [class_ "wrap"]
          (do h2_ "Post update"
              url <- ask
              form_
                [action_ (url PostR), method_ "POST"]
                (do relaxHtmlT
                      (Forge.view
                         (Forge.verified
                            (postUpdateForm
                               (do condition <- memberCondition
                                   title <- pure memberTitle
                                   pure UpdateInfo {condition, title, desc = ""}))))
                    p_ (button_ "POST UPDATE")))))

postPostR :: Handler (Html ())
postPostR = do
  (groupId, memberId) <- getSessionInfo
  Group {..} <-
    runDB
      (do grp <- get404 groupId
          pure grp)
  (inputs, _files) <- runRequestBody
  let inputMap =
        (M.fromListWith
           (<>)
           (map (first Forge.Key . second (pure . Forge.TextInput)) inputs))
  case runIdentity (Forge.generate inputMap (Forge.verified (postUpdateForm Nothing))) of
    Forge.Generated {generatedView = html, generatedValue = v} ->
      case v of
        Failure _errs ->
          htmlWithUrl
            (layoutWrapper
               (div_
                  [class_ "wrap"]
                  (do h2_
                        (do "Post update for "
                            toHtml groupPostcode)
                      url <- ask
                      form_
                        [action_ (url PostR), method_ "POST"]
                        (do relaxHtmlT html
                            p_ (button_ "POST UPDATE")))))
        Success UpdateInfo {condition, title, desc} -> do
          now <- liftIO getCurrentTime
          runDB
            (do insert_
                  Message
                    { messageMember = memberId
                    , messageGroup = groupId
                    , messageCondition = condition
                    , messageTitle = title
                    , messageDesc = desc
                    , messageCreated = now
                    }
                update
                  memberId
                  [ MemberCondition =. pure condition
                  , MemberTitle =. title
                  , MemberUpdated =. now
                  ])
          redirect DashboardR

getDashboardR :: Handler (Html ())
getDashboardR = do
  (groupId, memberId) <- getSessionInfo
  (Group {..}, Member {..}, updates) <-
    runDB
      (do grp <- get404 groupId
          mem <- get404 memberId
          updates <- generateUpdates groupId grp mem memberId
          pure (grp, mem, updates))
  htmlWithUrl
    (layoutWrapper
       (div_
          [class_ "wrap"]
          (do h2_
                (do "Dashboard for "
                    toHtml groupPostcode)
              url <- ask
              form_
                [action_ (url LogoutR), method_ "POST"]
                (p_ (button_ "logout"))
              p_
                (do "Your personal login code is: "
                    strong_ (code_ (toHtml memberCode))
                    " (Don't lose this.)")
              p_
                (do a_ [href_ (url PostR)] "Post an update"
                    " | "
                    a_ [href_ (url PassR)] "Change password"
                    " | "
                    a_ [href_ (url GenerateR)] "Generate codes")
              hr_ []
              updates)))

generateUpdates groupId group member memberId = do
  updates <- selectList [MessageGroup ==. groupId] [Desc MessageCreated]
  pure
    (if null updates
       then do
         url <- ask
         p_
           (do "There are no updates yet. Please "
               a_ [href_ (url PostR)] "post an update"
               "!")
       else do
         url <- ask
         when
           (not
              (any
                 (\Message {messageMember} -> messageMember == memberId)
                 (map entityVal updates)))
           (ul_
              [class_ "inline-errors"]
              (li_
                 (do "You haven't posted an update of your condition yet! Please "
                     a_ [href_ (url PostR)] "post an update"
                     "!")))
         div_
           [class_ "grid"]
           (forM_
              (dedup updates)
              (\(Entity mid Message {..}) ->
                 div_
                   [ class_
                       (if memberId == messageMember
                          then "update yours"
                          else "update")
                   ]
                   (do h3_ (toHtml messageTitle)
                       p_
                         (do strong_ "Condition: "
                             toHtml (show messageCondition))
                       p_ (toHtml messageDesc)
                       p_ (em_ (toHtml (show messageCreated)))))))
  where
    dedup =
      sortBy (flip (comparing entityKey)) .
      M.elems .
      M.fromListWith (flip const) .
      map (\m@(Entity mid Message {messageMember}) -> (messageMember, m))

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
                      , memberCondition = Nothing
                      , memberTitle = ""
                      , memberJoined = pure now
                      , memberUpdated = now
                      , memberPassword = Nothing
                      , memberOrigin = Nothing
                      }
                  pure (gid, memberId))
          setSession "nearly_groupId" (T.pack (show (fromSqlKey groupId)))
          setSession "nearly_memberId" (T.pack (show (fromSqlKey memberId)))
          redirect PassR

getHomeR :: Handler (Html ())
getHomeR = do
  minfo <- getSessionInfoMaybe
  case minfo of
    Nothing -> staticHome
    Just (_groupdId, _memberId) ->
        redirect DashboardR

staticHome :: HandlerFor App (Html ())
staticHome =
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
                            [action_ (url LoginR), method_ "POST"]
                            (do relaxHtmlT
                                  (Forge.view (Forge.verified loginForm))
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
                 p_ "The only data we will collect and publish is how many cases of each condition (asymptomatic, isolating, recovered) for each post code."
                 p_ (strong_ "Isolating.org is NOT a government-run web site.")
                 h3_ "Why are codes needed?"
                 p_
                   "To provide privacy for communities. You can only get a code if a group creator \
                                           \as provided you with one, via your letterbox or handed to you in person. This \
                                          \protects against abuse."
                 p_
                   "We do not accept phone numbers or email addresses in order \
                               \to protect the privacy of participants.")))

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

loginForm :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError (Text, Text)
loginForm =
  (,) <$>
  wrapErrorsAllowBubble
    "Postcode"
    (wrapHtml
       (\html ->
          p_
            (do label_ "Postcode: "
                html))
       postcodeField) <*>
  wrapErrorsAllowBubble
    "Member Code"
    (wrapHtml
       (\html ->
          p_
            (do label_ "Member Code: "
                html))
       (requiredTextField Nothing))

createGroupForm :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
createGroupForm =
  wrapErrorsAllowBubble
    "Postcode"
    (wrapHtml
       (\html -> p_
                   (do label_ "Postcode: "
                       html))
       postcodeField)

savePassForm :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
savePassForm =
  wrapErrorsAllowBubble
    "Password"
    (wrapHtml
       (\html -> p_
                   (do label_ "Password: "
                       html))
       requiredPasswordField)

enterPassForm :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
enterPassForm =
  wrapErrorsAllowBubble
    "Password"
    (wrapHtml
       (\html -> p_
                   (do label_ "Password: "
                       html))
       requiredPasswordField)

data UpdateInfo = UpdateInfo
  { condition :: Condition
  , title :: Text
  , desc :: Text
  }

postUpdateForm :: Maybe UpdateInfo -> Forge.Form index Identity (Html ()) Forge.Field IsolatingError UpdateInfo
postUpdateForm minfo =
  UpdateInfo <$>
  wrapErrorsAllowBubble
    "Condition"
    (wrapHtml
       (\html ->
          p_
            (do label_ "Condition: "
                html))
       (dropdownField
          (fmap (\UpdateInfo {condition} -> condition) minfo)
          (fmap
             (\x -> (x, T.pack (show x)))
             (pure Asymptomatic <> pure Isolating <> pure Recovered)))) <*>
  wrapErrorsAllowBubble
    "Address/Name"
    (wrapHtml
       (\html -> do
          p_
            (do label_ "Address/Name: "
                html)
          p_ "E.g. Flat 12, or Mrs Robinson. You don't have to put your name.")
       (requiredTextField (fmap (\UpdateInfo {title} -> title) minfo))) <*>
  wrapErrorsAllowBubble
    "Comment"
    (wrapHtml
       (\html -> do
          p_ (label_ "Comment (optional, no longer than 128 chars): ")
          p_ html)
       optionalTextareaField)

--------------------------------------------------------------------------------
-- Fields

postcodeField :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
postcodeField =
  Forge.ParseForm
    (\text ->
       if T.all (\c -> isAlphaNum c) (T.filter (not . isSpace) text)
         then pure (Right (T.strip text))
         else pure (Left InvalidPostCode))
    (requiredTextField Nothing)

--------------------------------------------------------------------------------
-- Forge utilities

data IsolatingError
  = LucidError Forge.Error
  | TextNotProvided
  | TextTooLong
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
    TextTooLong -> "Text too long. No more than 128 chars."
    ContextedError label e -> do
      toHtml label
      ": "
      showError e
    LucidError err ->
      case err of
        Forge.MissingInput {} -> "Missing input: try resubmitting the form?"
        Forge.InvalidInputFormat {} -> "Invalid input format."

requiredTextField :: Maybe Text -> Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
requiredTextField mdef =
  Forge.ParseForm
    (\t ->
       pure
         (if T.null t
            then Left TextNotProvided
            else Right t))
    (Forge.FieldForm Forge.DynamicFieldName (Forge.TextField mdef))

requiredPasswordField :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
requiredPasswordField =
  Forge.ParseForm
    (\t ->
       pure
         (if T.null t
            then Left TextNotProvided
            else Right t))
    (Forge.FieldForm Forge.DynamicFieldName (Forge.PasswordField Nothing))

requiredTextareaField :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
requiredTextareaField =
  Forge.ParseForm
    (\t ->
       pure
         (if T.null t
            then Left TextNotProvided
            else Right t))
    (Forge.FieldForm Forge.DynamicFieldName (Forge.TextareaField Nothing))

optionalTextareaField :: Forge.Form index Identity (Html ()) Forge.Field IsolatingError Text
optionalTextareaField =
  Forge.ParseForm
    (\t ->
       if T.length t < 128
         then pure (pure t)
         else pure (Left TextTooLong))
    (Forge.FieldForm Forge.DynamicFieldName (Forge.TextareaField Nothing))

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
       46
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

getSessionInfoMaybe :: Handler (Maybe (GroupId, MemberId))
getSessionInfoMaybe = do
  gid <- lookupSession "groupId"
  mid <- lookupSession "memberId"
  case (,) <$> (gid >>= (readMaybe . T.unpack)) <*> (mid >>= (readMaybe . T.unpack)) of
    Just (g, m) -> pure (pure (toSqlKey g, toSqlKey m))
    Nothing -> pure Nothing

getNearlySessionInfo :: Handler (GroupId, MemberId)
getNearlySessionInfo = do
  gid <- lookupSession "nearly_groupId"
  mid <- lookupSession "nearly_memberId"
  case (,) <$> (gid >>= (readMaybe . T.unpack)) <*> (mid >>= (readMaybe . T.unpack)) of
    Just (g, m) -> pure (toSqlKey g, toSqlKey m)
    Nothing -> redirect LoginR
