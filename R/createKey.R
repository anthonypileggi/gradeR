#' Create an Answer Key
#'
#' @param key_name Full path to the assignment name.  ("_KEY.Rdata" will be appended)
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

createKey <- function(key_name = "homework", vars = ls(name = ".GlobalEnv")){

  # Store answers in a list
  answers <- lapply(vars, get)
  names(answers) <- vars

  # Save the key as an .Rdata file
  save(answers, file = paste0(key_name, "_KEY.Rdata"))

}


