% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImFoR.R
\name{ImFoR}
\alias{ImFoR}
\title{Non-Linear Height Diameter Models for Forestry}
\usage{
ImFoR(data, train_frac = 0.8)
}
\arguments{
\item{data}{Datasets}

\item{train_frac}{Train-Test fraction}
}
\value{
\itemize{
  \item metrics: Metrics of all applied models
  \item plot:  Plot
}
}
\description{
Non-Linear Height Diameter Models for Forestry
}
\examples{
library("ImFoR")
data <- system.file("extdata", "data_test.csv", package = "ImFoR")
data_test <- read.csv(data)
Model<-ImFoR(data =data_test)
}
\references{
\itemize{
\item Jeelani, M.I., Tabassum, A., Rather, K and Gul,M.2023. Neural Network Modeling of Height Diameter Relationships for Himalayan Pine through Back Propagation Approach. Journal of The Indian Society of Agricultural Statistics. 76(3): 169–178
\item Tabassum, A., Jeelani, M.I., Sharma,M., Rather, K R ., Rashid, I and Gul,M.2022.  Predictive Modelling of Height and Diameter Relationships of Himalayan Chir Pine . Agricultural Science Digest - A Research Journal. DOI:10.18805/ag.D-5555
\item	Huang, S., Titus, S.J., and Wiens, D.P. 1992. Comparison of nonlinear height-diameter functiond for major Alberta tree species. Can J. For. Res. 22: 1297-1304. DOI : 10.1139/x92-172
\item -	Zeide, B. 1993. Analysis of growth equations. Forest Science 39(3):594-616. doi:10.1093/forestscience/39.3.594
}
}
