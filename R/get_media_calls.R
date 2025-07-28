#' @title Extract Paths and Filenames of All Media Calls in Rmd File
#' @description Prints paths and filenames of all media calls found in Rmd file into the console to be available for copy and paste operations.
#'
#' @param rmd_file  Path and filename of the Rmarkdown file to be processed. Rmd file must be present in the current working directory.
#'
#' @return  Character vector of path/filename of extracted media calls prints in the console
#' @import xml2
#' @import stringr
#' @examples
#' rmd_file = system.file("extdata", "sample-slides.rmd", package = "xtractr")
#' get_media_calls(rmd_file)
#' @export
get_media_calls <- function(rmd_file) {

  # Read the content of the Rmarkdown file

  rmd_content <- readLines(rmd_file, warn = FALSE)

  # Combine the lines into a single string

  rmd_text <- paste(rmd_content, collapse = "\n")

  # Extract all links using regular expressions

  links1 <- stringr::str_extract_all(rmd_text, "\\bhttps?://\\S+\\b")
  links2 <- stringr::str_extract_all(rmd_text, 'url\\(\\s*[\'"]?([^\\)"\']+)')

  # Combine the links
  all_links <- c(unlist(links1), unlist(links2))

  # Parse the Rmarkdown content as HTML

  rmd_html <- xml2::read_html(rmd_text)

  # Extract links from image tags (img src)

  img_links <- xml2::xml_attr(xml2::xml_find_all(rmd_html, ".//img"), "src")

  # Extract links from anchor tags (a href)

  a_links <- xml2::xml_attr(xml2::xml_find_all(rmd_html, ".//a"), "href")

  # Combine all links

  all_links <- c(all_links, img_links, a_links)

  # Filter the links to get only .gif, .mov, .mp4, .png, .jpg, .jpeg, and .html files

  target_extensions <- c(".gif", ".mov", ".mp4", ".png", ".jpg", ".jpeg", ".html")

  filtered_links <- all_links[stringr::str_detect(all_links, paste0(target_extensions, collapse = "|"))]

  # Print the list of links

  print(unique(filtered_links))
}

