% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rSARP.r
\name{tracking}
\alias{tracking}
\title{\code{tracking} - Tracking 525s completion within a Search}
\usage{
tracking(title = "", directory = "Searches")
}
\arguments{
\item{title}{- string that represents the Name of the Search. Used to label the graph with the search name.}

\item{directory}{- a string representing the sub directory within the search directory where the results will be stored for this analysis.  Defaults to the R working directory. The function will create a new directory if the default is not accepted.}
}
\value{
This function returns a graphic showing a bar chart analysis of 525s within a search by area covered and POS achieved
}
\description{
\code{tracking()} This function lives to track 525 progress towards completion for a search. Running this function after the tracking.csv
file has been updated produces a graph that displays the progress of search by 525.
}
\details{
tracking() is a tracking function built to track 525 progress graphically for reports to the SM / PIO. The height of each bar defines the number of searchers
working on this task. The location of the triangle relative to the top of the bar indicates the % completion of the task, so if the triangle is half way up the bar, the task is 50% complete.
}
\examples{
tracking(title="Midland County Search B")
}
\author{
John F. Hutcheson
}

