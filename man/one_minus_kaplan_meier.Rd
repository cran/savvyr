% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one_minus_kaplan_meier.R
\name{one_minus_kaplan_meier}
\alias{one_minus_kaplan_meier}
\title{One Minus Kaplan-Meier}
\usage{
one_minus_kaplan_meier(data, tau)
}
\arguments{
\item{data}{(\code{data.frame})\cr with columns including
\itemize{
\item \code{time_to_event}: Time to the first AE, death or soft competing event.
\item \code{type_of_event}: 0 for censored, 1 for AE, 2 for death, 3 for soft competing event.
}}

\item{tau}{(\code{number})\cr milestone at which One Minus Kaplan-Meier is computed.}
}
\value{
(\code{vector})\cr with the following entries:
\itemize{
\item \code{ae_prob}: Estimated probability of AE.
\item \code{ae_prob_var}: Variance of that estimate.
}
}
\description{
This function calculates the one minus Kaplan-Meier estimator of
adverse events (while censoring all competing events) observed in \verb{[0, tau]}.
Please also refer to formula (4) in \insertCite{stegherr_meta_analytic_2021;textual}{savvyr}.
}
\examples{
set.seed(123)
dat <- generate_data(n = 5, cens = c(2, 5), haz_ae = 2, haz_death = 3, haz_soft = 5)
one_minus_kaplan_meier(dat, tau = 4)
}
\references{
\insertRef{stegherr_meta_analytic_2021}{savvyr}
}
