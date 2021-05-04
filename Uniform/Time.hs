----------------------------------------------------------------------
--
-- Module      :  Uniform.Time
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | a minimal set of time operations
-- at the moment only a wrapper to time
-- examples in TestingTime.hs
module Uniform.Time
  ( module Uniform.Time,
    module Uniform.Error,  
    EpochTime,
    UTCTime (..),
  )
where

import Data.Convertible (convert)
import Data.Time as T
  ( NominalDiffTime,
    UTCTime (..),
    addUTCTime,
    defaultTimeLocale,
    diffDays,
    diffUTCTime,
    formatTime,
    getCurrentTime,
    parseTimeM,
    parseTimeOrError,
    toGregorian,
  )
import Data.Time.Clock.POSIX
    ( getCurrentTime, posixSecondsToUTCTime )
import System.Posix.Types (EpochTime)
import Uniform.Error

year2000 :: UTCTime
year2000 = readDate3 "2000-01-01"
-- ^ may serve as zero in some applications

instance CharChains2 UTCTime Text where -- orphan instance
  show' = s2t . show

instance CharChains2 T.NominalDiffTime Text where
  show' = s2t . show

instance CharChains2 (Integer, Int, Int) Text where
  show' = s2t . show

instance IsString UTCTime where
  fromString = readNote "IsString UTCTime"

getCurrentTimeUTC :: ErrIO UTCTime
addSeconds :: Double -> UTCTime -> UTCTime
diffSeconds :: UTCTime -> UTCTime -> T.NominalDiffTime
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


readDate2 :: Text -> UTCTime
-- ^ read data in the Jan 7, 2019 format (no . after month)
readDate2 datestring =
  parseTimeOrError
    True
    defaultTimeLocale
    "%b %-d, %Y"
    (t2s datestring) ::
    UTCTime

readDate3 :: Text -> UTCTime
readDate3 dateText = case (readDateMaybe dateText) of
  Nothing -> errorT ["readDate3", dateText, "cannot be parsed"]
  Just t -> t

readDateMaybe :: Text -> Maybe UTCTime
-- ^ read data in various formats (but not 9.10.20 !)
readDateMaybe dateText =
  listToMaybe . catMaybes $
    [ shortMonth,
      longMonth,
      monthPoint,
      germanNumeralShort,
      germanNumeral,
      isoformat
    ]
  where
    shortMonth :: Maybe UTCTime
    shortMonth =
      parseTimeM
        True
        defaultTimeLocale
        "%b %-d, %Y"
        dateString ::
        Maybe UTCTime
    longMonth =
      parseTimeM
        True
        defaultTimeLocale
        "%B %-d, %Y"
        dateString ::
        Maybe UTCTime
    monthPoint =
      parseTimeM
        True
        defaultTimeLocale
        "%b. %-d, %Y"
        dateString ::
        Maybe UTCTime
    germanNumeral =
      parseTimeM
        True
        defaultTimeLocale
        "%-d.%-m.%Y"
        dateString ::
        Maybe UTCTime
    germanNumeralShort =
      parseTimeM
        True
        defaultTimeLocale
        "%-d.%-m.%y"
        dateString ::
        Maybe UTCTime
    isoformat =
      parseTimeM
        True
        defaultTimeLocale
        "%Y-%m-%d"
        dateString ::
        Maybe UTCTime

    dateString = t2s dateText

fromEpochTime' :: EpochTime -> UTCTime
fromEpochTime' et = posixSecondsToUTCTime (realToFrac et)
