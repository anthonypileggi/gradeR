Installation
------------

``` r
devtools::install_github(repo = "anthonypileggi/gradeR")
```

Introduction
------------

The `gradeR` package was originally designed for teachers that are incorporating Swirl in their lesson plans and want an easy way to track student progress. Functionally, it is a simple tool that compares two workspaces.

Workflow: Grading Swirl Assignments
-----------------------------------

#### Create an Answer Key

1.  Create a new folder & move into it: `dir.create("Lesson1"); setwd(file.path(getwd(), "Lesson1"))`
2.  Complete the desired Swirl lesson.
3.  Create the answer key: `gradeR::createKey("Lesson1")` ("\_KEY.Rdata" will be appended to the name)

#### Instructions for Students

1.  Clear the current working directory: `rm(list = ls())`
2.  Complete the Swirl lesson.
3.  Save (& submit) your workspace: `save.image("Lesson1_anthony.Rdata")`

#### Grade the Assignments

1.  Collect all student submissions (.Rdata) within the folder containing the answer key.
2.  Set that folder as the current working directory: `setwd(file.path(getwd(), "Lesson1"))`
3.  Identify the answer key: `id_key <- grepl("_KEY", list.files())`
4.  Grade the assignments: `gradeR::grade(list.files()[!id_key], list.files()[id_key])`
