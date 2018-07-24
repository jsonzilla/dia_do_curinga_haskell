import Test.HUnit
import System.IO
import Lib

test1 = TestCase (assertEqual "Is leap year" (True) (isLeapYear 2012))
test2 = TestCase (assertEqual "Is not leap year" (False) (isLeapYear 2011))
test3 = TestCase (assertEqual "Is not leap year int 2013" (0) (isLeapYearInt 2013))
test4 = TestCase (assertEqual "Is leap year int 2012" (1) (isLeapYearInt 2012))

test5 = TestCase (assertEqual "In date 2017.7.19 should be count 200 days" (200) (countDays (19,7,2017)))
test6 = TestCase (assertEqual "In date 2017.1.1 should be count 1 day" (1) (countDays (1,1,2017)))
test7 = TestCase (assertEqual "In date 2017.12.31 should be count 365 days" (365) (countDays (31,12,2017)))

test8 = TestCase (assertEqual "Gregorian day 200 should be Frode 140 day" (140) (fixDay 2012 200))
test9 = TestCase (assertEqual "Gregorian day 366 should be Frode 140 day" (306) (fixDay 2012 366))
test10 = TestCase (assertEqual "Gregorian day 365 should be Frode 140 day" (305) (fixDay 2012 365))
test11 = TestCase (assertEqual "Gregorian day 59 should be Frode 364 day" (364) (fixDay 2012 59))

test12 = TestCase (assertEqual "Gregorian day 60 should be Frode 0 (joker) day" (0) (fixDay 2012 60))
test13 = TestCase (assertEqual "Gregorian day 61 should be Frode 1 day" (1) (fixDay 2012 61))
test14 = TestCase (assertEqual "Gregorian day 62 should be Frode 2 day" (2) (fixDay 2012 62))

test15 = TestCase (assertEqual "year 1789 should be return 1 in conversion" (1) (fixYear 1789))
test16 = TestCase (assertEqual "year 1790 should be return 0 in conversion" (0) (fixYear 1790))
test17 = TestCase (assertEqual "year 1791 should be return 1 in conversion" (1) (fixYear 1791))

test18 = TestCase (assertEqual "day 62 in year 1790 should be return 1 : club" (1) (seasons 62 1790))
test19 = TestCase (assertEqual "day 154 in year 1790 should be return 2 : cup" (2) (seasons 154 1790))
test20 = TestCase (assertEqual "day 247 in year 1790 should be return 3 : swords" (3) (seasons 247 1790))
test21 = TestCase (assertEqual "day 338 in year 1790 should be return 0 : golds" (0) (seasons 338 1790))
test22 = TestCase (assertEqual "day 365 in year 1790 should be return 1 : club" (1) (seasons 365 1790))

test23 = TestCase (assertEqual "date 30 february 2012 should be not valid" (False) (feb 30 2012))
test24 = TestCase (assertEqual "date 29 february 2011 should be not valid" (False) (feb 29 2011))
test25 = TestCase (assertEqual "date 29 february 2012 should be valid" (True) (feb 29 2012))
test26 = TestCase (assertEqual "date 28 february 2013 should be valid" (True) (feb 28 2013))
test27 = TestCase (assertEqual "date 30 february 2011 should be not valid" (False) (feb 30 2011))

test28 = TestCase (assertEqual "date 28, 2, 2011 should be KE1O1P1P" ("IKE_I1O_I1P_I1P_") (diaDoCuringAbbr (28, 2, 2011)))
test29 = TestCase (assertEqual "date 1, 3, 2011  should be 1O1O1P1P" ("I1O_I1O_I1P_I1P_") (diaDoCuringAbbr (1, 3, 2011)))
test30 = TestCase (assertEqual "date 27, 2, 2012 should be QEKEKP2P" ("IQE_IKE_IKP_I2P_") (diaDoCuringAbbr (27, 2, 2012)))
test31 = TestCase (assertEqual "date 28, 2, 2012 should be KE1O1P2P" ("IKE_I1O_I1P_I2P_") (diaDoCuringAbbr (28, 2, 2012)))
test32 = TestCase (assertEqual "date 29, 2, 2012 should be Jo1O1P2P" ("IJoJ_I1O_I1P_I2P_") (diaDoCuringAbbr (29, 2, 2012)))
test33 = TestCase (assertEqual "date 1, 3, 2012 should be 1O1O1P2P"  ("I1O_I1O_I1P_I2P_") (diaDoCuringAbbr (1, 3, 2012)))
test34 = TestCase (assertEqual "date 28, 2, 2013 should be KE1O1P3P" ("IKE_I1O_I1P_I3P_") (diaDoCuringAbbr (28, 2, 2013)))
test35 = TestCase (assertEqual "date 1, 3, 2013 should be 1O1O1P3P"  ("I1O_I1O_I1P_I3P_") (diaDoCuringAbbr (1, 3, 2013)))
test36 = TestCase (assertEqual "date 28, 2, 2014 should be KE1O1P4P" ("IKE_I1O_I1P_I4P_") (diaDoCuringAbbr (28, 2, 2014)))
test37 = TestCase (assertEqual "date 1, 3, 2014 should be 1O1O1P4P"  ("I1O_I1O_I1P_I4P_") (diaDoCuringAbbr (1, 3, 2014)))
test38 = TestCase (assertEqual "date 19, 7, 2017 should be 10C8P6P7P"("I10C_I8P_I6P_I7P_") (diaDoCuringAbbr (19, 7, 2017)))

test39 = TestCase (assertEqual "date 10, 1, 1999 should be a valid date" (True) (validDate (10, 1, 1999)))
test40 = TestCase (assertEqual "date 0, 1, 1999 should be a invalid date" (False) (validDate (0, 1, 1999)))
test41 = TestCase (assertEqual "date 32, 1, 1999 should be a invalid date" (False) (validDate (32, 1, 1999)))
test42 = TestCase (assertEqual "date 29, 2, 2012 should be a valid date" (True) (validDate (29, 2, 2012)))
test43 = TestCase (assertEqual "date 29, 2, 2011 should be a invalid date" (False) (validDate (29, 2, 2011)))
test44 = TestCase (assertEqual "date 1, 0, 1999 should be a invalid date" (False) (validDate (1, 0, 1999)))
test45 = TestCase (assertEqual "date 1, 13, 1999 should be a invalid date" (False) (validDate (1, 13, 1999)))
test46 = TestCase (assertEqual "date 1, 12, 0    should be a invalid date" (False) (validDate (1, 12, 0)))
test47 = TestCase (assertEqual "date 1, 12, 1789 should be a valid date" (True) (validDate (1, 12, 1789)))
test48 = TestCase (assertEqual "date 1, 12, 1790 should be a valid date" (True) (validDate (1, 12, 1790)))
test49 = TestCase (assertEqual "date 1, 12, 9999 should be a valid date" (True) (validDate (1, 12, 9999)))
test50 = TestCase (assertEqual "date 1, 4, 9999 should be a valid date" (True) (validDate (1, 4, 9999)))

tests = TestList [test1, test2, test3, test4, test5, test6, test7,
  test8, test9, test10, test11, test12, test13, test14, test15, test16,
  test17, test18, test19, test20, test21, test22, test23, test24, test25,
  test26, test27, test28, test29, test30, test31, test32, test33, test34,
  test35, test36, test37, test38, test39, test40, test41, test42, test43,
  test44, test45, test46, test47, test48, test49, test50]


main = do runTestTT tests
