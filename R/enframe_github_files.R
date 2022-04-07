#' Create a (markdown ready) data frame from a GitHub file tree
#'
#' @param repo Repository name (excluding owner).
#' @param path Subdirectory inside repo.
#' @param regexp A regular expression passed on to [grep()] to filter paths.
#' @param owner GitHub organization (default is `curso-r`).
#'
#' @return A data frame with one column named `link`.
#'
#' @export
enframe_github_files <- function(repo, path, regexp, owner = "curso-r") {

  # Correct branch
  branch <- get_default_branch(repo, owner)

  # Extract files
  files <- list_github_files(repo, path, regexp)

  # Enframe and mutate
  df <- data.frame(link = files)
  df$link <- paste0(
    "[", df$link, "]",
    "(https://github.com/curso-r/", repo, "/blob/", branch, "/", df$link, ")"
  )

  return(df)
}
