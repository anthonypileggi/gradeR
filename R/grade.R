#' Grade based on R workspace equivalence
#'
#' @param assignment Full path to assignment (.Rdata) (character).  Can be a vector.
#' @param key Full path to key (.Rdata) (character).
#' @return Data.frame with the file names and corresponding grades.
#' @seealso \code{\link{createKey}}
#' @examples
#' \dontrun{
#' my_key <- file.path(getwd(), "Lesson1_KEY.Rdata")
#'
#' student_names <- c("Anthony", "Shannon", "Avery")
#' students <- file.path(getwd(), paste0("Lesson1_", student_names, ".Rdata"))
#'
#' grade(students, my_key, TRUE)
#' grade(students, my_key, FALSE)
#' }
#'
#' @export
#'

grade <- function(assignment, key, grade_for_completion = FALSE){

  # Load Answer Key
  load(key)

  # Load Students
  grades <- rep(NA, length(assignment))
  for (J in seq_along(assignment)) {

    # Load data for current student
    load(assignment[J])

    # Grading...
    number_correct <- 0
    for (I in seq_along(names(answers))) {

      cur_var <- names(answers)[I]

      if (exists(cur_var)) {

        if (grade_for_completion) {

          number_correct <- number_correct + 1

        } else {

          number_correct <- number_correct + identical(get(cur_var), answers[[I]])

        }

      }

    }

    grades[J] <- 100 * (number_correct / length(answers))

    # Purge workspace of student-created variables
    rm(list = names(answers))

  }

  return(data.frame(File = assignment,
                    Grade = grades))
}
