#' @title Extract PNG, JPG, JPEG, and BMP Media Calls Found in Rmd File, Transform to Rmarkdown \code{![]()} format, and Optionally Write to Disk
#' @description All PNG, JPG, JPEG, and BMP media calls found in the specified Rmd file are transformed into Rmarkdown \code{![]()} syntax. The list is sent to the console as originally found and in the Rmarkdown \code{![]()} format. If `write_to_file` is `TRUE` (default = FALSE) and 'output_file' argument has a text value, the list is saved to a new file with the specified `output_file` name. If `write_to_file` is `FALSE`, no file is written.
#'
#' @param rmd_file   An .Rmd path/filename provided as a character string.
#' @param output_file   An .Rmd path/filename provided as a character string. File is saved in the current working directory.
#' @param write_to_file   A logical value indicating whether to write the result to a file (default = FALSE).
#'
#' @return  Character vector object named 'imgs-only'.
#'
#' @examples
#' rmd_file <- system.file("extdata", "sample-slides.rmd", package = "xtractr")
#' # Only print to console, no file is saved
#' xtract_imgs_optional_save(rmd_file)
#'#' # Save to a file with a custom filename "custom-output.rmd"
#'\dontrun{
#' xtract_imgs_optional_save(rmd_file, output_file = "custom-output.rmd", write_to_file = TRUE)
#'}
#' @export
xtract_imgs_optional_save <- function(rmd_file, output_file, write_to_file = FALSE) {

  # Get transformed media calls from the Rmarkdown file:
  transformed_calls <- xtractr::transform_media_calls(rmd_file)

  # Filter out .img files and store them in imgs_only
  imgs_only <- transformed_calls[grepl("\\b\\S+\\.(jpg|jpeg|png|bmp)\\b", transformed_calls)]

  if (write_to_file) {
    # Write the imgs_only content to the output file if write_to_file is TRUE
    writeLines(imgs_only, con = output_file)
  }

  # Print the imgs_only content to the console
  cat(imgs_only, sep = "\n")

  # Return the transformed media calls
  return(imgs_only)
}
