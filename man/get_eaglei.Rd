% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_eaglei.R
\name{get_eaglei}
\alias{get_eaglei}
\title{Download Outage Data}
\usage{
get_eaglei(start, end)
}
\arguments{
\item{start}{start date to get data for}

\item{end}{end date to get date for}
}
\value{
raw eaglei data with est_cusomters generated with the procedure described in details
}
\description{
Downloadeds publically available EAGLE-I data published
by \href{https://www.nature.com/articles/s41597-024-03095-5}{Brelsford et al.}
}
\details{
\subsection{Customer Estimates}{

Customer estimates are generated by adding the number of households
(\href{https://censusreporter.org/tables/B11001/}{ACS} centered on year of
storm) and number of establishments
(\href{https://www.census.gov/programs-surveys/susb/data/tables.html}{Census SUSB}
for year of storm) per county, getting the percentage of households and
establishments per county by state, and multiplying the percentage by the
total number of customers in a state (\href{https://www.eia.gov/electricity/data/browser/#/topic/56?agg=0,1&geo=g&endsec=vg&linechart=ELEC.CUSTOMERS.US-ALL.A~ELEC.CUSTOMERS.US-RES.A~ELEC.CUSTOMERS.US-COM.A~ELEC.CUSTOMERS.US-IND.A&columnchart=ELEC.CUSTOMERS.US-ALL.A~ELEC.CUSTOMERS.US-RES.A~ELEC.CUSTOMERS.US-COM.A~ELEC.CUSTOMERS.US-IND.A&map=ELEC.CUSTOMERS.US-ALL.A&freq=A&start=2008&end=2023&ctype=map&ltype=pin&rtype=s&maptype=0&rse=0&pin=}{EIA}
year of storm)
}
}
\examples{
\dontrun{
harvey_eaglei <- get_eaglei(2017)
}
}
