-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Time
--
-- | a minimal set of time operations
-- at the moment only a wrapper to time
-- examples in TestingTime.hs
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
     MultiParamTypeClasses
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , ScopedTypeVariables
    , DeriveDataTypeable   -- needed
    , TypeSynonymInstances
    , DoAndIfThenElse
    , TypeFamilies
    , ConstraintKinds
    , BangPatterns
             #-}


module Uniform.Time (
        module Uniform.Time
        , module Uniform.Error   -- or at least ErrIO
        -- , module Uniform.Strings
        , UTCTime , EpochTime
--    , htf_thisModulesTests
        )  where

import Test.Framework

import Data.Time as T
import Uniform.Error
-- import Uniform.Strings
import Data.Convertible
import System.Posix.Types (EpochTime)
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

instance CharChains2 UTCTime Text where
    show' = s2t . show
instance CharChains2 T.NominalDiffTime Text where
    show' = s2t . show
instance CharChains2 (Integer, Int, Int) Text where
    show' = s2t . show


getCurrentTimeUTC :: ErrIO UTCTime
addSeconds :: Double -> UTCTime -> UTCTime
diffSeconds :: UTCTime -> UTCTime  -> T.NominalDiffTime

getCurrentTimeUTC = liftIO $ T.getCurrentTime
addSeconds s t = T.addUTCTime (realToFrac s) t
diffSeconds a b =  T.diffUTCTime a b

toYMD = T.toGregorian . T.utctDay
diffDays a b = T.diffDays (T.utctDay a) (T.utctDay b)

epochTime2UTCTime :: EpochTime -> UTCTime
epochTime2UTCTime = convert
