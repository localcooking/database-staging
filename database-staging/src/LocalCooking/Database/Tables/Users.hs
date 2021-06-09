{-# LANGUAGE
    TypeOperators
  , TypeApplications
  , OverloadedLabels
  , OverloadedStrings
  , DataKinds
  , GADTs
  , FlexibleContexts
  , DerivingStrategies
  , DeriveAnyClass
  , DeriveGeneric
  #-}

module LocalCooking.Database.Tables.Users where

import Data.Int (Int32)
import Data.UUID (UUID)
import Data.Time (UTCTime)
import Data.Maybe (isJust)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Squeal.PostgreSQL
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC


octetLength :: null 'PGbytea --> null 'PGint4
octetLength = unsafeFunction "octet_length"

genRandomUUID :: Expr (null 'PGuuid)
genRandomUUID = UnsafeExpression "gen_random_uuid()"

type Schema =
  '[ "users"                        ::: UsersTable
   , "active_users"                 ::: ActiveUsersView
   , "pending_registrations"        ::: PendingRegistrationsTable
   , "active_pending_registrations" ::: ActivePendingRegistrationsView
   , "sessions"                     ::: SessionsTable
   ]

type UsersTable =
  'Table
    ('[ "pk_users"        ::: 'PrimaryKey '["user_id"]
      , "uq_email"        ::: 'Unique '["email"]
      , "password_length" ::: 'Check '["password"]
      , "salt_length"     ::: 'Check '["salt"]
      , "continuity"      ::: 'Check '["created_on", "last_login", "last_active", "deactivated_on"]
      ] :=>
     '[ "user_id"        ::: 'Def   :=> 'NotNull 'PGint4
      , "email"          ::: 'NoDef :=> 'NotNull ('PGvarchar 255)
      , "password"       ::: 'NoDef :=> 'NotNull 'PGbytea
      , "salt"           ::: 'NoDef :=> 'NotNull 'PGbytea
      , "created_on"     ::: 'Def   :=> 'NotNull 'PGtimestamptz
      , "last_login"     ::: 'NoDef :=> 'Null 'PGtimestamptz
      , "last_active"    ::: 'NoDef :=> 'Null 'PGtimestamptz
      , "deactivated_on" ::: 'NoDef :=> 'Null 'PGtimestamptz
      ])

type ActiveUsersView =
  'View
     '[ "user_id"        ::: 'NotNull 'PGint4
      , "email"          ::: 'NotNull ('PGvarchar 255)
      , "password"       ::: 'NotNull 'PGbytea
      , "salt"           ::: 'NotNull 'PGbytea
      , "created_on"     ::: 'NotNull 'PGtimestamptz
      , "last_login"     ::: 'Null 'PGtimestamptz
      , "last_active"    ::: 'Null 'PGtimestamptz
      , "deactivated_on" ::: 'Null 'PGtimestamptz
      ]

type PendingRegistrationsTable =
  'Table
    ('[ "uq_email"      ::: 'Unique '["email"]
      , "pk_auth_token" ::: 'PrimaryKey '["auth_token"]
      ] :=>
     '[ "email"      ::: 'NoDef :=> 'NotNull ('PGvarchar 255)
      , "auth_token" ::: 'Def :=> 'NotNull 'PGuuid
      , "expiration" ::: 'Def :=> 'NotNull 'PGtimestamptz
      ]
    )

type ActivePendingRegistrationsView =
  'View
    '[ "email"      ::: 'NotNull ('PGvarchar 255)
     , "auth_token" ::: 'NotNull 'PGuuid
     , "expiration" ::: 'NotNull 'PGtimestamptz
     ]

type SessionsTable =
  'Table
    ('[ "fk_user_id"  ::: 'ForeignKey '["user_id"] "public" "users" '["user_id"]
      , "pk_sessions" ::: 'PrimaryKey '["session_token"]
      ] :=>
     '[ "user_id"       ::: 'NoDef :=> 'NotNull 'PGint4
      , "session_token" ::: 'Def   :=> 'NotNull 'PGuuid
      , "expiration"    ::: 'Def   :=> 'NotNull 'PGtimestamptz
      ])


deactivateUser :: MonadUnliftIO m => Int32 -> PQ (Public Schema) (Public Schema) m ()
deactivateUser userId = transactionallyRetry defaultMode $ do
  manipulateParams_ revokeSessions (Only userId)
  manipulateParams_ setDeactivation (Only userId)
  where
    revokeSessions :: Manipulation_ (Public Schema) (Only Int32) ()
    revokeSessions =
      deleteFrom_
        #sessions
        (#user_id .== param @1 @('NotNull 'PGint4))
    setDeactivation :: Manipulation_ (Public Schema) (Only Int32) ()
    setDeactivation =
      update_
        #users
        (Set currentTimestamp `as` #deactivated_on)
        (#user_id .== param @1 @('NotNull 'PGint4))


expirePendingRegistrations :: Manipulation_ (Public Schema) () ()
expirePendingRegistrations =
  deleteFrom_
    #pending_registrations
    (#expiration .< currentTimestamp)

findExistingNotDeactivatedUser' :: VarChar 255 -> PQ (Public Schema) (Public Schema) m Bool
findExistingNotDeactivatedUser' email = do
  rows <- runQueryParams findExistingNotDeactivatedUser (Only email)
  -- isJust <$> firstRow rows
  pure True
  where
    findExistingNotDeactivatedUser :: Query_ (Public Schema) (Only (VarChar 255)) (Only Int32)
    findExistingNotDeactivatedUser =
      select_ (1 `as` #fromOnly) $ from (table #users) & where_
        (#email .== param @1 @('NotNull ('PGvarchar 255)) .&& (#deactivated_on & isNull))

assignRegistration :: MonadPQ (Public Schema) m
                   => MonadUnliftIO m
                   => VarChar 255 -> m (Maybe UUID)
assignRegistration email = do -- transactionallyRetry defaultMode $ do
  doesExist <- findExistingNotDeactivatedUser' email
  if doesExist
    then pure Nothing
    else firstRow =<< manipulateParams addRegistration (Only email)
  where
    addRegistration :: Manipulation_ (Public Schema) (Only (VarChar 255)) (Only UUID)
      -- '[ 'NotNull ('PGvarchar 255) ] '[ "auth_token" ::: 'NotNull 'PGuuid ]
    addRegistration =
      insertInto #pending_registrations
        ( Values_ (Set (param @1) `as` #email :* Default `as` #auth_token :* Default `as` #expiration)
            -- (from (subquery #users) & where_ (not_ $ exists findExistingNotDeactivatedUser))
        )
        OnConflictDoRaise
        (Returning_ (#auth_token `as` #fromOnly))


definition :: Definition (Public '[]) (Public Schema)
definition =
  users >>>
  activeUsers >>>
  pendingRegistrations >>>
  activePendingRegistrations >>>
  sessions
  where
    users = createTableIfNotExists #users
      (  serial `as` #user_id
      :* (varchar & notNullable) `as` #email
      :* (bytea & notNullable) `as` #password
      :* (bytea & notNullable) `as` #salt
      :* (timestamptz & notNullable & default_ currentTimestamp) `as` #created_on
      :* (timestamptz & nullable) `as` #last_login
      :* (timestamptz & nullable) `as` #last_active
      :* (timestamptz & nullable) `as` #deactivated_on
      )
      (  primaryKey #user_id `as` #pk_users
      :* unique #email `as` #uq_email
      :* check #password (octetLength #password .== 64) `as` #password_length
      :* check #salt (octetLength #salt .== 32) `as` #salt_length
      :* check (#created_on :* #last_login :* #last_active :* #deactivated_on)
          ( #created_on .<= #last_login
            .&& #last_login .<= #last_active
            .&& #last_active .<= #deactivated_on
          ) `as` #continuity
      )

    activeUsers = createOrReplaceView #active_users
      (  select Star (from (table #users) & where_ (#deactivated_on & isNull))
      )

    pendingRegistrations = createTableIfNotExists #pending_registrations
      (  (varchar & notNullable) `as` #email
      :* (uuid & notNullable & default_ genRandomUUID) `as` #auth_token
      :* (timestamptz & notNullable & default_ (currentTimestamp !+ interval_ 1 Days)) `as` #expiration
      )
      (  unique #email `as` #uq_email
      :* primaryKey #auth_token `as` #pk_auth_token
      )

    activePendingRegistrations = createOrReplaceView #active_pending_registrations
      (  select Star (from (table #pending_registrations) & where_ (#expiration .>= currentTimestamp))
      )

    sessions = createTableIfNotExists #sessions
      (  (int & notNullable) `as` #user_id
      :* (uuid & notNullable & default_ genRandomUUID) `as` #session_token
      :* (timestamptz & notNullable & default_ (currentTimestamp !+ interval_ 1 Days)) `as` #expiration
      )
      (  foreignKey #user_id #users #user_id (OnDelete Restrict) (OnUpdate Cascade) `as` #fk_user_id
      :* primaryKey #session_token `as` #pk_sessions
      )
