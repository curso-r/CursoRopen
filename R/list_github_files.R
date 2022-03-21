#' List files in a GitHub repository
#'
#' @param repo Repository name (excluding owner).
#' @param path Subdirectory inside repo.
#' @param regexp A regular expression passed on to [grep()] to filter paths.
#' @param owner GitHub organization (default is `curso-r`).
#'
#' @return A string vector of paths.
#' @examples list_github_files("main-web-scraping", "slides/", "[0-9]-.*html$")
#'
#' @export
list_github_files <- function(repo, path = NULL, regexp = NULL,
                              owner = "curso-r") {

  # Get default branch
  branch <- gh::gh(
    "/repos/{owner}/{repo}",
    owner = owner, repo = repo
  )$default_branch

  # Get file tree
  tree <- gh::gh(
    "/repos/{owner}/{repo}/git/trees/{branch}",
    owner = owner, repo = repo, branch = branch,
    recursive = 1
  )$tree

  # Replace NULLs
  if (is.null(path)) path <- ""
  if (is.null(regexp)) regexp <- ""

  # Filter tree given path and regexp
  tree <- Filter(function(x) substr(x$path, 1, nchar(path)) == path, tree)
  tree <- Filter(function(x) grepl(regexp, x$path), tree)

  # Return paths
  sapply(tree, function(x) x$path)
}
