#' listar arquivos em um repositorio no github
#'
#' @param repo  nome do repositorio
#' @param dir nome do diretorio
#' @param pattern padrao para buscar arquivos
#' @param org organizacao no GitHub. Por padrao eh a curso-r.
#'
#' @return um vetor
#' @export
#'
#' @examples list_github_files(repo = "main-web-scraping", dir = "slides/", pattern = "[0-9]-.*html$")

list_github_files <- function(repo, dir = NULL, pattern = NULL, org = "curso-r") {

  req <- httr::GET(
    paste0(
      "https://api.github.com/repos/",  org, "/",
      repo,
      "/git/trees/master?recursive=1"
    )
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
