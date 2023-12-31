% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embed_mp4.R
\name{embed_mp4}
\alias{embed_mp4}
\title{Creates a Complete HTML Video Tag for Embedding into an Rmarkdown Document}
\usage{
embed_mp4(mp4_path, output_file = NULL)
}
\arguments{
\item{mp4_path}{The path to the MP4 video file (local file path or URL) as a character string.}

\item{output_file}{An optional output file path. If specified, the HTML video tag will be appended to this file.}
}
\value{
A complete HTML video tag as a character string.
}
\description{
This function takes the path/filename information input by the user and generates a complete HTML video tag for embedding into an Rmarkdown file.  The completed HTML video tag is output to the console for further copy and paste operations.  OPTIONALLY, the output can be appended to a file specified by the user, or saved to a new file specified by the user.  The output will be appended to the existing document at the location of the cursor.  Credit to Mitchell O'Hara-Wild for the first iteration of this function  (https://www.mitchelloharawild.com/).
}
\details{
This function takes the path/filename information provided by the user and generates a complete HTML video tag suitable for embedding in an Rmarkdown file. The resulting HTML code can be printed to the console or appended to a specified file.
}
\examples{
\dontrun{
# Embed an MP4 video and print to console
embed_mp4("path/to/video.mp4")

# Append to a specific file
embed_mp4("path/to/video.mp4", "output.Rmd")

# Append to the default file (custom-output.rmd)
embed_mp4("path/to/video.mp4", "custom-output.rmd")

# Print to console
embed_mp4("path/to/video.mp4")
}


}
