#' @title correctLPJmL_new
#' @description Convert LPJmL content (dummy function)
#' @param x magpie object provided by the read function
#'
#' @author Kristine Karstens
#' @seealso
#' [readLPJmL_new()]
#' @examples
#' \dontrun{
#' readSource("LPJmL_new", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#'

correctLPJmL_new <- function(x) { # nolint: object_name_linter.

  x <- toolConditionalReplace(x, conditions = "<0", replaceby = 0)

  return(x)
}
