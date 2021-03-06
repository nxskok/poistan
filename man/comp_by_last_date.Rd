% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/get_data.R
\name{comp_by_last_date}
\alias{comp_by_last_date}
\title{Competitions whose last scheduled game is after input date}
\usage{
comp_by_last_date(lim_date)
}
\arguments{
\item{lim_date}{Date that matches must be after}
}
\value{
Data frame: \code{date} is date of last scheduled match in league,
  \code{comp} is competition number, \code{name} is name of competition
}
\description{
Competitions whose last scheduled game is after input date
}

