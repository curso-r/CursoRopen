#' List files in a GitHub repository
#'
#' @param repo Repository name.
#' @param dir Folder name.
#' @param pattern Regex pattern to look for.
#' @param org GitHub organization (default is `curso-r`).
#' @param pat Add PAT as request header. This reduces the chances of getting an
#'   empty response.
#'
#' @return A string vector.
#' @examples list_github_files("main-web-scraping", "slides/", "[0-9]-.*html$")
#'
#' @export
list_github_files <- function(repo, dir = NULL, pattern = NULL, org = "curso-r", pat = FALSE) {

  if (pat) {
    usethis::ui_info("Tentando pegar seu pat com gitcreds::gitcreds_get()...")
    my_pat <- gitcreds::gitcreds_get()$password
    if (is.null(my_pat)) {
      usethis::ui_oops("Não consegui :(")
      usethis::ui_info("Tentando pegar seu pat Sys.getenv('GITHUB_PAT')")
      my_pat <- Sys.getenv('GITHUB_PAT')
      if (my_pat == "") usethis::ui_stop("Não consegui encontrar seu PAT.")
    }
    hh <- httr::add_headers(authorization = paste("Bearer", my_pat))
  } else {
    hh <- httr::add_headers()
  }

  req <- httr::GET(
    paste0(
      "https://api.github.com/repos/",  org, "/",
      repo,
      "/git/trees/master?recursive=1"
    ),
    hh
  )

  arquivos <- unlist(
    lapply(httr::content(req)$tree, "[", "path"),
    use.names = FALSE
  )

  if (!is.null(dir)) {
    arquivos <- grep(dir, arquivos, value = TRUE, fixed = TRUE)
  }

  if (!is.null(pattern)) {
    arquivos <- arquivos[grep(pattern, arquivos)]
  }

  arquivos
}
