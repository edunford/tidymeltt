% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_event_data.R
\name{simulate_event_data}
\alias{simulate_event_data}
\title{simulate_event_data}
\usage{
simulate_event_data(N = 100, known.events = 4, known.episodes = 0,
  known.episodes.events = 0, n.datasets = 4, time.distort = 1,
  space.distort = 1, episode.window = 5, min_date = as.Date("1990-01-01"),
  max_date = as.Date("1991-01-22"), lat.bounds = c(-90, 90),
  lon.bounds = c(-180, 180), taxonomies = data.frame(k = c(rep(1, 4), rep(2,
  3), rep(3, 4)), depth = c(10, 5, 3, 2, 13, 6, 3, 15, 10, 5, 2)))
}
\arguments{
\item{N}{simulated sample size}

\item{known.events}{number of known to event-event matches}

\item{known.episodes}{number of known episode-episode matches}

\item{known.episodes.events}{number of known episode-event matches}

\item{n.datasets}{number of simulated datasets}

\item{time.distort}{time distortion around the known values}

\item{space.distort}{spatial distortion around the known values; spatial
units are in kilometers}

\item{episode.window}{define the maximum time window from which the simulated episodal entries are drawn.}

\item{min_date}{minimum (lower bound) on the simulated date range}

\item{max_date}{maximum (upper bound) on the simulated date range}

\item{lat.bounds}{latitude for the spatial boundaries of the simulated
location}

\item{lon.bounds}{longitude for the spatial boundaries of the simulated
location}

\item{taxonomies}{data.frame of number of taxonomy dimensions (k) and depths
(depth).}
}
\value{
a list continating N number of simulated datasets along with with a taxonomy to disambiguate.
}
\description{
Simulate multiple event datasets given a specified date and spatial range
along with disambiguating meta data and taxonomy dimensions. Data simulates
the type of data generating process tidymeltt is designed for.
}
\examples{

sim.data = simulate_event_data(N=200,known.events = 4,known.episodes = 0,
                     known.episodes.events = 0,
                     n.datasets = 4,time.distort = 1,space.distort = 0.05,
                     episode.window = 10,
                     min_date = as.Date("2001-01-01"),max_date =as.Date("2001-02-01"),
                     lat.bounds = c(-1,1),lon.bounds =c(-1,1),
                     taxonomies = data.frame(k=c(rep(1,4),rep(2,3),rep(3,4)),
                                              depth = c(10,5,3,2, 13,6,3, 15,10,5,2)))

tax = sim.data$user.taxonomies
D1 = sim.data$user.data \%>\% filter(dataset=="D1")
D2 = sim.data$user.data \%>\% filter(dataset=="D2")
D3 = sim.data$user.data \%>\% filter(dataset=="D3")
D4 = sim.data$user.data \%>\% filter(dataset=="D4")




}
