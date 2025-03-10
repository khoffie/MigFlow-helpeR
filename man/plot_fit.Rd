% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_fit}
\alias{plot_fit}
\title{Easy plotting of model fit for individual flows}
\usage{
plot_fit(dt, x, y, th_min, smooth = TRUE, th_max = NULL, p_sample = NULL)
}
\arguments{
\item{dt}{data.table with flows and prediction}

\item{x}{x-axis variable}

\item{y}{y-axis variable}

\item{th_min}{numeric, minimum prediction}

\item{smooth}{logical, if TRUE ggplots::geom_smooth uses a GAM to
show center of data.}

\item{th_max}{numeric, maximum prediction}

\item{p_sample}{numeric, optional, if specified a sample of
fraction p_sample is shown.}
}
\value{
plot
}
\description{
Easy plotting of model fit for individual flows
}
\author{
Konstantin Hoffie
}
