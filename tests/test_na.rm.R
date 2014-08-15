test_that("default of TRUE for na.rm works.", {
  x <- c(1:10,NA)
  expect_that( mean(x), equals(5.5))
  expect_that( sd(x), equals(5.5))
  expect_that( range(x), equals(c(1,10)))
  expect_that( mean(NA), equals(NaN) )
  expect_that( median(NA), equals(NA) )
})
test_that("mosaic formula interface works.", {
  require(mosaic); 
  require(dplyr)
  expect_that( mean( length ~ sex, data=KidsFeet ),
               is_equivalent_to(c(25.10500,24.32105263)) )
  KidsFeet %>% group_by(sex) %>% summarize(mean(length)) -> Test
  expect_that( Test, is_a("data.frame"))
  expect_that( Test[[2]],
               is_equivalent_to(c(25.10500,24.32105263)))
  expect_that( cor(x, y, data=data.frame(x=c(1:3,NA),y=c(3:1,4))), 
                   equals(-1 ) )
})