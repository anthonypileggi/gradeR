Installation
------------

``` r
devtools::install_github(repo = "anthonypileggi/gradeR")
```

Introduction
------------

The `gradeR` package was originally designed for teachers that are incorporating Swirl in their lesson plans and want an easy way to track student progress. Functionally, it is a simple tool that compares two workspaces.

Proposed Workflow for Grading Swirl Assignments
-----------------------------------------------

### Teacher

1.  Create a new folder & move into it: `dir.create("Lesson1"); setwd(file.path(getwd(), "Lesson1"))`

2.  Complete the desired Swirl lesson.

3.  Create the answer key: `gradeR::createKey("Lesson1")` ("\_KEY.Rdata" will be appended to the name)

4.  Store all student submissions (.Rdata) in this folder

5.  Grade the assignments: `gradeR::grade()`

### Student

1.  Clear the current working directory: `rm(list = ls())`

2.  Complete the Swirl lesson.

3.  Save (& submit) your workspace: `save.image("Lesson1_anthony.Rdata")`

``` r
library(gradeR)
```
