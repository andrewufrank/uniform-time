-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Time
--
-- | a minimal set of time operations
-- at the moment only a wrapper to time
-- examples in TestingTime.hs
-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds          #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse        #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances           #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Uniform.Time (
        module Uniform.Time
        , module Uniform.Error   -- or at least ErrIO
        -- , module Uniform.Strings
        , EpochTime, UTCTime (..)
        -- , fromEpochTime'
--    , htf_thisModulesTests
        )  where

-- import Test.Framework

import Data.Time as T
import Uniform.Error
-- import Uniform.Strings
import Data.Convertible (convert)
import System.Posix.Types (EpochTime)
--import System.Time (getClockTime, toCalendarTime, calendarTimeToString)
import Data.Time.Clock.POSIX


year2000 :: UTCTime
year2000 = readDate3 "2000-01-01"
-- serves as zero in some applications

--class Times a where
--    type TimeUTC  a
--    type YMD a
--
--    getCurrentTimeUTC :: ErrIO a
--    addSeconds :: Double -> a ->  a
--    diffSeconds :: a -> a -> T.NominalDiffTime
--
--    toYMD :: a -> YMD a
--    diffDays :: a -> a -> Integer
--
--instance Times UTCTime where
--    type TimeUTC UTCTime =  T.UTCTime
--    type YMD UTCTime = (Integer, Int, Int)

instance CharChains2 UTCTime Text where  -- orphan instance
    show' = s2t . show
instance CharChains2 T.NominalDiffTime Text where
    show' = s2t . show
instance CharChains2 (Integer, Int, Int) Text where
    show' = s2t . show

instance IsString UTCTime where
  fromString = readNote "IsString UTCTime"

getCurrentTimeUTC :: ErrIO UTCTime
addSeconds :: Double -> UTCTime -> UTCTime
diffSeconds :: UTCTime -> UTCTime  -> T.NominalDiffTime

getCurrentTimeUTC = liftIO T.getCurrentTime
addSeconds s t = T.addUTCTime (realToFrac s) t
diffSeconds = T.diffUTCTime

toYMD :: UTCTime -> (Integer, Int, Int)
toYMD = T.toGregorian . T.utctDay
diffDays :: UTCTime -> UTCTime -> Integer
diffDays a b = T.diffDays (T.utctDay a) (T.utctDay b)

epochTime2UTCTime :: EpochTime -> UTCTime
epochTime2UTCTime = convert

getDateAsText :: ErrIO Text
getDateAsText = callIO $ do
            now <- getCurrentTime
            let res = formatTime defaultTimeLocale "%b %-d, %Y" now
            return . s2t $ res
--                        t <-  getClockTime
--                        tc <- toCalendarTime t
--                        return (s2t $ calendarTimeToString tc)

readDate2 :: Text ->  UTCTime
-- ^ read data in the Jan 7, 2019 format (no . after month)
readDate2 datestring = parseTimeOrError True defaultTimeLocale
            "%b %-d, %Y" (t2s datestring) :: UTCTime

readDate3 :: Text -> UTCTime
readDate3 dateText = case (readDateMaybe dateText) of
    Nothing -> errorT   ["readDate3",dateText, "cannot be parsed"]
    Just t -> t

readDateMaybe :: Text -> Maybe  UTCTime
-- ^ read data in various formats (but not 9.10.20 !)
readDateMaybe dateText  = listToMaybe . catMaybes $
        -- headNote "readDateMaybe werwerxx" . dropWhile isNothing $
            [shortMonth, longMonth, monthPoint
            , germanNumeralShort, germanNumeral, isoformat]

        -- fromJust $ shortMonth >>= longMonth

    -- case shortMonth of
    --     Just t -> Just t
    --     Nothing -> case longMonth of
    --         Just t2 -> Just t2
    --         Nothing -> case monthPoint of
    --             Just t3 -> Just t3
    --             Nothing -> case germanNumeralShort of
    --               Just t4 -> Just t4
    --               Nothing -> case germanNumeral of
    --                 Just t5 -> Just t5
    --                 Nothing ->  isoformat

    where
        shortMonth :: Maybe UTCTime
        shortMonth = parseTimeM True defaultTimeLocale
            "%b %-d, %Y" dateString :: Maybe UTCTime
        longMonth = parseTimeM True defaultTimeLocale
            "%B %-d, %Y" dateString :: Maybe UTCTime
        monthPoint = parseTimeM True defaultTimeLocale
            "%b. %-d, %Y" dateString :: Maybe UTCTime
        germanNumeral = parseTimeM True defaultTimeLocale
            "%-d.%-m.%Y" dateString :: Maybe UTCTime
        germanNumeralShort = parseTimeM True defaultTimeLocale
            "%-d.%-m.%y" dateString :: Maybe UTCTime
        isoformat = parseTimeM True defaultTimeLocale
            "%Y-%m-%d" dateString :: Maybe UTCTime

        dateString = t2s dateText

fromEpochTime' :: EpochTime -> UTCTime
fromEpochTime' et = posixSecondsToUTCTime (realToFrac et)
-- from Distribution.Hackage.DB.Utility from hackage-db
