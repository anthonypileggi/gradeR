#' Grade based on R workspace equivalence
#'
#' @param assignment Full path to assignment (.Rdata) (character).  Can be a vector.
#' @param key Full path to key (.Rdata) (character).
#' @param grade_for_completion Should grading be based on completion or accuracy (default = FALSE).
#' @return Data.frame with the file names and corresponding grades.
#' @seealso \code{\link{createKey}}
#' @examples
#' \dontrun{
#' # Answer key
#' my_key <- file.path(getwd(), "Lesson1_KEY.Rdata")
#'
#' # Student assignments
#' student_names <- c("Anthony", "Shannon", "Avery")
#' students <- file.path(getwd(), paste0("Lesson1_", student_names, ".Rdata"))
#'
#' # Grade all students for completion
#' grade(students, my_key, TRUE)
#'
#' # Grade all students for accuracy
#' grade(students, my_key)
#' }
#'
#' @export
#'

grade <- function(assignment, key){

  # Load Answer Key
  load(key)

  # Load Students
  completion <- rep(NA, length(assignment))
  accuracy <- rep(NA, length(assignment))
  for (J in seq_along(assignment)) {

    # Clean workspace
    rm(list = ls()[!grepl("assignment|key|answers|completion|accuracy|J", ls())])

    # Load data for current student
    load(assignment[J])

    # Grading
    number_exist <- 0
    number_correct <- 0
    for (I in seq_along(names(answers))) {

      cur_var <- names(answers)[I]

      if (exists(cur_var)) {

        number_exist <- number_exist + 1

        if (identical(get(cur_var), answers[[I]])) {

          number_correct <- number_correct + 1

        }

      }

    }

    completion[J] <- 100 * (number_exist / length(answers))
    accuracy[J] <- 100 * (number_correct / length(answers))

    # Purge workspace of student-created variables
    suppressWarnings(rm(list = names(answers)))

  }

  return(data.frame(File = assignment,
                    Completion = completion,
                    Accuracy = accuracy))
}
