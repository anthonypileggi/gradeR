library(gradeR)

context("Full test of the gradeR package")

# For returning R to the original state
orig_dir <- getwd()

# Create a folder for Lesson0
dir.create("Lesson0")
setwd(file.path(getwd(), "Lesson0"))

# Clean current working directory
#rm(list = ls())

# Complete a Swirl "Lesson"
x <- 10
y <- rep(9, 10)
z <- matrix(1:10, 2, 5)

print(ls())

# Create Answer Key
createKey("Lesson0", c("x", "y", "z"))

# Load Answer Key
load(list.files())

# Testing (Round 1)
test_that("Answer key is a list", {
  expect_equal(class(answers), class(list()))
})

test_that("Answer key contains correct values", {
  expect_identical(x, answers$x)
  expect_identical(y, answers$y)
  expect_identical(z, answers$z)
})

#-----------------------------------------------
# (1) Create student data (good & bad)
for(I in 1:100){
  rm(list = ls()[!grepl("orig_dir", ls())])

  x <- 10

  if (runif(1) > 0.5){
    y <- rep(9, 10)   # right
  } else {
    y <- rep(7, 3)    # wrong
  }

  if (runif(1) > .5) { z <- matrix(1:10, 2, 5) }

  save(list = ls()[!grepl("orig_dir", ls())],
       file = paste0("Lesson0_", paste0(sample(letters, 5), collapse = ""), ".Rdata"))
}

# (2) Grading
student_files <- list.files()[!grepl("_KEY", list.files())]
grades <- grade(student_files, "Lesson0_KEY.Rdata")

# Testing (Round 2)
test_that("Completion is correct.", {
  expect_equal(min(grades$Completion), 100 * (2/3))
  expect_equal(max(grades$Completion), 100)
})

test_that("Accuracy is correct.", {
  expect_equal(min(grades$Accuracy), 100 * (1/3))
  expect_equal(max(grades$Accuracy), 100)
})

#-----------------------------------------------
# Return R to original state
setwd(orig_dir)
unlink("Lesson0", recursive = TRUE)