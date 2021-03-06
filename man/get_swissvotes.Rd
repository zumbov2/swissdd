% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_swissvotes.R
\name{get_swissvotes}
\alias{get_swissvotes}
\title{Download additional data collected by annee politique suisse (the complete SwissVotes-Database)}
\usage{
get_swissvotes(DB = T, savecitation = F, codebook = F)
}
\arguments{
\item{DB}{get database}

\item{savecitation}{by default = FALSE. Saves the citation within a .txt file in the working directory if TRUE.}

\item{codebook}{by default = FALSE. If TRUE navigates your browser to the codebook.}
}
\value{
a tibble containing the results
}
\description{
\code{get_swissvotes} downloads additional data collected by annee politique suisse. It allows for completely downloading their database. Please cite data.
}
\details{
get_swissvotes - retrieve data on votes. The unit of analysis are votes.
}
\examples{


# See codebook only
 get_swissvotes(codebook=FALSE)



}
