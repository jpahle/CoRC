% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entity_accessors.R
\name{getGlobalQuantityReferences}
\alias{getGlobalQuantityReferences}
\title{Get global quantity references}
\usage{
getGlobalQuantityReferences(key = NULL, model = getCurrentModel())
}
\arguments{
\item{key}{Optionally, a character vector specifying which global quantities to get.}

\item{model}{A model object.}
}
\value{
Global quantities and associated references, as data frame.
}
\description{
\code{getGlobalQuantityReferences} returns global quantity attribute references as a data frame.
}
\details{
The \href{https://jpahle.github.io/CoRC/articles/entity_management.html}{online article on managing model entities} provides some further context.
}
\seealso{
\code{\link{getGlobalQuantities}} \code{\link{setGlobalQuantities}}

Other global quantity functions: 
\code{\link{deleteGlobalQuantity}()},
\code{\link{entity_finders}},
\code{\link{getGlobalQuantities}()},
\code{\link{newGlobalQuantity}()},
\code{\link{setGlobalQuantities}()}
}
\concept{global quantity functions}
