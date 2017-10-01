library(testthat)

#test make_filename function
expect_that(make_filename(2013), matches("accident_2013.csv.bz2"))
