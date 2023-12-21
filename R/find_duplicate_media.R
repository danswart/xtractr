#' @title Find Duplicate Media Calls in Rmd Slide Show (if any) as Aid to Removing Duplicates; Optionally, Save to File in Current Directory
#' @description Finds duplicate media calls in Rmd slide deck (if any) and reports them in the console as aid to removing duplicates.  Optionally, will save duplicates to file in current directory.  User supplies argument for Rmd path/filename containing the slide deck, and the (optional) logical argument of TRUE.  Default value = FALSE.    CAUTION - WHEN 'save_to_file' argument = TRUE, A NEW FILE IS SAVED IN THE CURRENT DIRECTORY NAMED 'media-duplicates.rmd'.
#'
#' Confirmation message of saved file appears in the console when 'save_to_file' argument = TRUE.  Otherwise, list of duplicates (if any) are written to the console.
#'
#' @param rmd_file   An .Rmd path/filename provided as a character string.  Default value = "sample-slides.Rmd"
#' @param save_to_file   A logical vector,provided as a character string, indicating user request to write a new file to current working directory.  Default value = "FALSE"
#'
#' @return  An R object named 'dupliate_media' which is printed in console, or (optionally) written to a new Rmd file in current working directory.
#'
#' @examples
#' rmd_file = system.file("extdata", "sample-slides.rmd", package = "xtractr")
#' find_duplicate_media(rmd_file)
#'
#' \dontrun{
#'    find_duplicate_media("path/to/your/sample-slides.rmd", save_to_file = TRUE)
#'}
#'
#' @export
find_duplicate_media <- function(rmd_file = "sample-slides.rmd", save_to_file = FALSE) {
  # Read the content of the Rmd file
  content <- readLines(rmd_file, warn = FALSE)

  # Regular expression to match various media file references
  media_pattern <- "([\\w/\\.-]+\\.(gif|mp4|mov|png|jpeg|jpg))"

  # Extract media references
  media_refs <- regmatches(content, gregexpr(media_pattern, content, perl = TRUE))
  media_refs <- unlist(media_refs)

  # Find duplicates
  duplicated_media <- media_refs[duplicated(media_refs)]

  # Unique duplicates
  unique_duplicates <- unique(duplicated_media)

  # Save to file if specified
  if (save_to_file) {
    writeLines(unique_duplicates, "media-duplicates.rmd")
    cat("Duplicate media calls have been stored in 'media-duplicates.rmd'\n")
  } else {
    cat("Duplicate media calls:\n")
    cat(unique_duplicates, sep = "\n")
  }

  return(invisible(NULL))
}

