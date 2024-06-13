-- |
-- Module      : Database.PostgreSQL.Simple.MigrationTest
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : andre@andrevdm.com
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of postgresql-migration specifications.

{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Simple.TransactionPerStepTest where

import           Data.Foldable                        (traverse_)
import           Database.PostgreSQL.Simple           (Connection, execute_)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationOptions (..),
                                                       MigrationResult (..),
                                                       TransactionControl (..),
                                                       runMigration,
                                                       runMigrations,
                                                       defaultOptions)
import           Database.PostgreSQL.Simple.Util      (existsTable)
import           Test.Hspec                           (Spec, describe, it, shouldBe, shouldThrow, shouldReturn, anyException, afterAll_)

migrationSpec :: Connection -> Spec
migrationSpec con = afterAll_ cleanup $ describe "Migrations" $ do
  let
    scriptCreate2 = MigrationScript "create2.sql" "create table trn2 (c2 varchar);"
    scriptError2 = MigrationScript "errorScript.sql" "ALSO NOT SQL!"

  it "asserts that the test table does not exist" $ do
    r <- existsTable con "trn1"
    r `shouldBe` False

  it "asserts that the test table does not exist" $ do
    r <- existsTable con "trn2"
    r `shouldBe` False

  it "validates an initialization on an empty database" $ do
    r <- runMigration' (MigrationValidation MigrationInitialization)
    r `shouldBe` MigrationError "No such table: schema_migrations"

  it "initializes a database" $ do
    r <- runMigration' MigrationInitialization
    r `shouldBe` MigrationSuccess

  it "creates the schema_migrations table" $ do
    r <- existsTable con "schema_migrations"
    r `shouldBe` True

  -- This test shoud thow an exception since the second migration script is invalid SQL
  it "executes a migration script" $ (`shouldThrow` anyException) $ do
    let opts = defaultOptions{optTransactionControl = TransactionPerStep}
    r <- runMigrations con opts [scriptCreate2, scriptError2]
    r `shouldBe` MigrationSuccess

  -- Since TransactionPerStep was used the first migration should have been committed even though the second one failed
  it "creates the table from the executed script" $ do
    r <- existsTable con "trn2"
    r `shouldBe` True

  it "does not allow SQL injection via table name option" $ do
    let opts = defaultOptions{optTableName = "tricked_you (gotcha int); --"}
    runMigrations con opts [MigrationInitialization] `shouldReturn` MigrationSuccess
    existsTable con "tricked_you" `shouldReturn` False
    existsTable con "tricked_you (gotcha int); --" `shouldReturn` True

  where
    runMigration' =
      runMigration con defaultOptions{optTransactionControl = NoNewTransaction}

    cleanup =
      traverse_ (execute_ con)
        [ "drop table if exists trn2"
        , "drop table if exists schema_migrations"
        , "drop table if exists \"tricked_you (gotcha int); --\""
        ]
