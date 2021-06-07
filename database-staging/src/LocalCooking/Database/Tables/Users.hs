{-# LANGUAGE
    TypeOperators
  , TypeApplications
  , OverloadedLabels
  , OverloadedStrings
  , DataKinds
  , GADTs
  , FlexibleContexts
  #-}

module LocalCooking.Database.Tables.Users where

import Data.Int (Int32)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Squeal.PostgreSQL


octetLength :: null 'PGbytea --> null 'PGint4
octetLength = unsafeFunction "octet_length"

genRandomUUID :: Expr (null 'PGuuid)
genRandomUUID = UnsafeExpression "gen_random_uuid()"

type Schema =
  '[ "users" ::: UsersTable
   , "sessions" ::: SessionsTable
   ]

type UsersTable =
  'Table
    ('[ "pk_users" ::: 'PrimaryKey '["user_id"]
      , "uq_email" ::: 'Unique '["email"]
      , "password_length" ::: 'Check '["password"]
      , "salt_length" ::: 'Check '["salt"]
      , "continuity" ::: 'Check '["created_on", "last_login", "last_active", "deactivated_on"]
      ] :=>
     '[ "user_id" ::: 'Def :=> 'NotNull 'PGint4
      , "email" ::: 'NoDef :=> 'NotNull ('PGvarchar 255)
      , "password" ::: 'NoDef :=> 'NotNull 'PGbytea
      , "salt" ::: 'NoDef :=> 'NotNull 'PGbytea
      , "created_on" ::: 'Def :=> 'NotNull 'PGtimestamptz
      , "last_login" ::: 'NoDef :=> 'Null 'PGtimestamptz
      , "last_active" ::: 'NoDef :=> 'Null 'PGtimestamptz
      , "deactivated_on" ::: 'NoDef :=> 'Null 'PGtimestamptz
      ])

type SessionsTable =
  'Table
    ('[ "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["user_id"]
      , "pk_sessions" ::: 'PrimaryKey '["session_token"]
      ] :=>
     '[ "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
      , "session_token" ::: 'Def :=> 'NotNull 'PGuuid
      , "expiration" ::: 'Def :=> 'NotNull 'PGtimestamptz
      ])

-- looks like we can't declare volatile, stable, etc.
-- type DeactivateUserFn =
--   'Function ( '[ 'Null ])

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


definition :: Definition (Public '[]) (Public Schema)
definition =
  users >>>
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

    sessions = createTableIfNotExists #sessions
      (  (int & notNullable) `as` #user_id
      :* (uuid & notNullable & default_ genRandomUUID) `as` #session_token
      :* (timestamptz & notNullable & default_ (currentTimestamp !+ interval_ 1 Days)) `as` #expiration
      )
      (  foreignKey #user_id #users #user_id (OnDelete Restrict) (OnUpdate Cascade) `as` #fk_user_id
      :* primaryKey #session_token `as` #pk_sessions
      )
