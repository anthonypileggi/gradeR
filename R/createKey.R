#' Create an Answer Key
#'
#' @param lesson_name Full path to the assignment name.  ("_KEY.Rdata" will be appended)
#' @param vars Names of the variables where answers are stored.  (default = all variables in current workspace)
#' @return An .Rdata file containing the list 'answers' with solutions.
#' @seealso \code{\link{grade}}
#' @examples
#' \dontrun{
#' x <- 9
#' y <- 1:10
#' z <- rep(0,5)
#'
#' # Save all variables as 'test_KEY.Rdata'
#' createKey("test")
#'
#' # Save only x & y as 'test_KEY.Rdata'
#' createKey("test", vars = c("x", "y"))
#' }
#'
#' @export
#'

createKey <- function(lesson_name = "homework", vars = NA){

  # If vars is not specified, include all variables in current workspace
  vars <- ifelse(is.na(vars), ls(), vars)

  # Store answers in a list
  answers <- lapply(vars, get)
  names(answers) <- vars

  # Save the key as an .Rdata file
  save(answers, file = paste0(lesson_name, "_KEY.Rdata"))

}


