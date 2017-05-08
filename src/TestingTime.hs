-----------------------------------------------------------------------------
--
-- Module      :   top tests for time
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

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

module Main     where


import Uniform.Strings

import {-@ HTF_TESTS @-} Uniform.Time

import Test.Framework

main = do
    putIOwords ["HTF LayoutTest.hs:\n posTest"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end LayoutTest.hs:\n posTest"]
    return r

timeTest ::   IO [Bool]
timeTest = do
    r <- runErr timeTest2
    v1 <- case r of
        Left msg -> do
                putIOwords ["errorTest returned Left :", msg]
                return [False]
        Right v -> return v
    return v1




timeTest2 ::   ErrIO [Bool]
-- examples how to use
timeTest2 = do
    t1 <- getCurrentTimeUTC
    putIOwords ["now",   showT t1]
    let     t2 = addSeconds 4.0 t1
            d4 = diffSeconds t2 t1
            x1 = d4 == 4.0
    putIOwords ["diff d4", showT d4, showT t2]

    let day1 = toYMD t1


    let     tomorrow = addSeconds (24 * 60 * 60) t1
            day2 = toYMD tomorrow
            oneDay = diffDays  tomorrow t1
            x2 = oneDay == 1

    putIOwords ["today is", showT day1, "tomorrow", showT day2]


    return [x1, x2, True]




