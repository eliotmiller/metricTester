\name{quadratPlacer}
\alias{quadratPlacer}
\title{Randomly place quadrats in arena}
\usage{
  quadratPlacer(no.quadrats, x_max, y_max, quadrat_size)
}
\arguments{
  \item{no.quadrats}{Number of quadrats to place}

  \item{x_max}{Maximum x bounds of arena}

  \item{y_max}{Maximum y bounds of arena}

  \item{quadrat_size}{Size of desired quadrat}
}
\value{
  A matrix with the X & Y coordinates of the four corners
  of each quadrat placed
}
\description{
  Given a desired number of quadrats, the arena size, and
  the quadrat size, will attempt to place quadrats down in
  a non-overlapping fashion
}
\details{
  Places quadrats down in non-overlapping fashion according
  to parameters supplied. Will run indefinitely if
  unacceptable parameters are supplied, but will not crash.
}
\examples{
bounds <- quadratPlacer(no.quadrats=15, x_max=300, y_max=300, quadrat_size=50)
}
\references{
  Miller, Trisos and Farine.
}

