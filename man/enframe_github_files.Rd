% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enframe_github_files.R
\name{enframe_github_files}
\alias{enframe_github_files}
\title{Create a (markdown ready) data frame from a GitHub file tree}
\usage{
enframe_github_files(repo, path, regexp, owner = "curso-r")
}
\arguments{
\item{repo}{Repository name (excluding owner).}

\item{path}{Subdirectory inside repo.}

\item{regexp}{A regular expression passed on to \code{\link[=grep]{grep()}} to filter paths.}

\item{owner}{GitHub organization (default is \code{curso-r}).}
}
\value{
A data frame with one column named \code{link}.
}
\description{
Create a (markdown ready) data frame from a GitHub file tree
}
