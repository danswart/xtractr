#' @title Convert Rmd Slide Show to Bookdown Compatible Text for Further Editing
#' @description Rmd Slide Show file is transformed to single line of bookdown compatible text and stored in new Rmd file in the current working directory.  User supplies argument for Rmd path/filename containing the slide deck, and the argument for the output path/filename.  Default values are "sample-slides-copy.Rmd" and "sample-slides-to-bookdown.Rmd".
#'
#' Confirmation messages appear in the console.
#'
#' @param rmd_file   An .Rmd path/filename provided as a character string.  Default value = "sample-slides-copy.Rmd"
#' @param txt_output_file An Rmd path/filename provided as a character string.  Default value = "sample-slides-to-bookdown.Rmd"
#'
#' @return  An R object named modified_text which is written to a new Rmd file, suitable for editing in bookdown.  New file is created in current working directory.
#'
#' @import renderthis
#' @import tools
#' @import pdftools
#'
#' @examples
#' \dontrun{
#'    slides_to_bookdown_txt("sample-slides-copy.rmd", "sample-slides-to-bookdown.Rmd")
#'}
#'
#' @export
slides_to_bookdown_txt <- function(rmd_file = "sample-slides-copy.rmd", txt_output_file = "sample-slides-to-bookdown.Rmd") {

  # Generate PDF from the Rmd file
  renderthis::to_pdf(rmd_file)

  # Get the base name of the Rmd file (without extension)
  rmd_base_name <- tools::file_path_sans_ext(basename(rmd_file))

  # Specify the path to the generated PDF file
  pdf_file_path <- paste0(rmd_base_name, ".pdf")

  # Read the text content from the PDF
  pdf_text_content <- pdftools::pdf_text(pdf_file_path)

  # Specify the path for the output text file
  text_file <- paste0(rmd_base_name, ".txt")

  # Write the text content to the text file
  writeLines(pdf_text_content, con = text_file)

  # Confirmation message
  cat("Text content has been extracted and saved to", text_file, "\n")

  # Read the .txt File
  txt_content <- readLines(text_file, warn = FALSE)

  # Concatenate all lines into one continuous line
  modified_text <- paste(txt_content, collapse = " ")

  # Remove extra spaces and line breaks, but keep spaces between words
  modified_text <- gsub("\\s+", " ", modified_text)

  # Write the Modified Content Back to the .txt File
  writeLines(modified_text, con = txt_output_file)

  # Confirmation message
  cat("Slide file has been converted into text and stored in an Rmd file with no more than two spaces between words in", txt_output_file, "\n")
}



