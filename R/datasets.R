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

UDG.datasets <- function(which = "", pattern = "") {
if (which == "") which <- c("OBSERVATIONS", "REANALYSIS", "CMIP5", "CORDEX")
  df <- lapply(which, function(x) read.csv(file.path(find.package("climate4R.UDG"), paste0("datasets_", x,".txt")), stringsAsFactors = FALSE)[ ,1:3])
  df <- do.call("rbind", df)
  df[grep(pattern, df$name, ignore.case = TRUE),]
}

