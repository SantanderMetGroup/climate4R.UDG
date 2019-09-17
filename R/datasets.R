#' @title Show UDG datasets
#' @description Access the installed user's vocabulary
#' @param which Which group of datasets. Choices are: "OBSERVATIONS", "REANALYSIS", "CMIP5", "CORDEX". 
#' By default all are diplayed.
#' @param pattern Optional. Pattern in the dataset name as passed to function \code{\link{grep}} (case-insensitive).
#' @return The datasets table, in the form of a \code{data.frame}
#' @note The function assumes that the user has read permission to the package installation directory
#' @author M Iturbide
#' @export
#' @importFrom utils read.csv
#' @examples
#' # Default built-in datasets
#' str(UDG.datasets())
#' (sets <- UDG.datasets())
#' sets[grep("CORDEX-EUR44.*historical", sets$name), ]
#' # Pointing to a specific group of datasets
#' UDG.datasets(which = "CORDEX")$name
#' # Using argument pattern
#' UDG.datasets(pattern = "CORDEX-EUR44.*historical")$name

UDG.datasets <- function(pattern = "", full.info = FALSE) {
  lf <- list.files(file.path(find.package("climate4R.UDG")), pattern = "datasets.*.txt", full.names = TRUE)
  df <- lapply(lf, function(x) read.csv(x, stringsAsFactors = FALSE)[ ,1:3])
  names(df) <- gsub(lf, pattern = ".*/datasets_|.txt", replacement = "")
  matchlist <- lapply(df, function(x) x[grep(pattern, x$name, ignore.case = TRUE),])
  sublist <- matchlist[unlist(lapply(matchlist, function(x) nrow(x) != 0))]
  if (pattern != "") message("Matches found for: ", names(sublist))
  if (!full.info) message("Label names are returned, set argument full.info = TRUE to get more information")
  if (full.info) {
    return(sublist)
  } else {
    return(lapply(sublist, function(x) x[["name"]]))
  }
}

