#' Update YAML front matter of a RMarkdown file
#'
#' @param path Path to file.
#' @param ... Name-value pairs to update in front matter.
#' @param new_path Path to output file. If `NULL`, prints output to console.
#'
#' @return A string vector with the updated content.
#'
#' @export
change_rmd_yaml <- function(path, ..., new_path = NULL) {

  # Read file
  lines <- readLines(path)

  # Find dashes
  dashes <- grep("^---\\s*$", lines)
  stopifnot(length(dashes) == 2)

  # Read front matter as YAML
  yaml <- yaml::yaml.load(lines[(dashes[1] + 1):(dashes[2] - 1)])

  # Append name-value pairs to the end of front matter
  dots <- list(...)
  yaml <- yaml::as.yaml(c(yaml[setdiff(names(yaml), names(dots))], dots))

  # Reform file
  out <- c("---", strsplit(yaml, "\n")[[1]], lines[-seq_len(dashes[2] - 1)])

  # Print contents (if requested)
  if (is.null(new_path)) {
    return(out)
  }

  # Write to new file
  writeLines(out, new_path)
}
