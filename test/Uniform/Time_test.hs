-----------------------------------------------------------------------------
--
-- Module      :   top tests for time
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Uniform.Time_test     where



import Data.Time 
import Uniform.Time {-@ HTF_TESTS @-} 
import Test.Framework
--import System.Time (calendarTimeToString)
-- import Text.Show.Pretty
import Uniform.Strings
import Uniform.Time as UT hiding (ppShow)
-- import Uniform.Pretty 




test_date1 =
  do
    let dateString = "26 Jan 2012 10:54 AM"
    let timeFromString = parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime
    -- Format YYYY/MM/DD HH:MM
    print $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M" timeFromString
    -- Format MM/DD/YYYY hh:MM AM/PM
    let res = formatTime defaultTimeLocale "%m/%d/%Y %I:%M %p" timeFromString
    assertEqual "01/26/2012 10:54 AM" res

test_date2 = do
    -- now for a string with single digit months and days:
    let dateString = "9-8-2012 10:54 AM"
    let timeFromString = parseTimeOrError True defaultTimeLocale "%-d-%-m-%Y %l:%M %p" dateString :: UTCTime
    -- Format YYYY/MM/DD HH:MM
    let res = formatTime defaultTimeLocale "%Y/%m/%d %H:%M" timeFromString
--    let res = formatTime defaultTimeLocale "%b %d, %Y" timeFromString
    assertEqual "2012/08/09 10:54" res

test_date3 =
    assertEqual "2019-01-07 00:00:00 UTC" (showT $ readDate2 "Jan 7, 2019")

test_date4 = do
    let nowT = readDate2 "Jan 7, 2019"
    now <- getCurrentTime
    let res = formatTime defaultTimeLocale "%b %-d, %Y" nowT
    assertEqual "Jan 7, 2019" res

--test_date5= do
--    r1 :: ErrOrVal Text <- runErr $ getDateAsText
--    let r2 = fromRightNote "werw" r1
--    assertEqual "Mon Mar 11 17:37:12 CET 2019" r2

test_date3a =
    assertEqual r3a (map (showT . readDate3) t3a)

r3a =
    ["2019-01-03 00:00:00 UTC", "2019-01-03 00:00:00 UTC",
     "2019-03-05 00:00:00 UTC", "2019-01-06 00:00:00 UTC",
     "2011-02-07 00:00:00 UTC", "2018-01-01 00:00:00 UTC",
     "2020-10-09 00:00:00 UTC", "2018-11-27 00:00:00 UTC",
     "2009-05-14 00:00:00 UTC", "2019-01-04 00:00:00 UTC"]
t3a = ["Jan 3, 2019", "Jan. 3, 2019", "March 5, 2019"
    , "jan 6, 2019", "February 7, 2011"
    , "1.1.2018", "9.10.20", "2018-11-27", "2009-05-14"
    , "Jan. 4, 2019"]


test_showRead =
--    do
--        print time1
--        print show1
--        print read1
        assertEqual read1 time1

    where
     dateString = "9-8-2012 10:54 AM"
     time1 :: UTCTime
     time1 = parseTimeOrError True defaultTimeLocale "%-d-%-m-%Y %l:%M %p" dateString :: UTCTime
     show1 = show time1 :: String
     read1 = read show1 :: UTCTime

test_show = assertEqual (timeX) 
        (readNote "test_showT" timeXshow :: UTCTime)
timeXshow = show timeX :: String 

-- test_pp = assertEqual (show timeX) (ppShow timeX)
-- demonstrates the issue with ppShow which cannot be 
-- read 

-- test_pp2 = assertEqual "" (ppShow timeX)
-- just to produce a display of "2012-08-09 10:54:00 UTC"

timeX :: UTCTime
timeX = parseTimeOrError True defaultTimeLocale "%-d-%-m-%Y %l:%M %p"
                "9-8-2012 10:54 AM" :: UTCTime

-- test_force = assertBool False 
