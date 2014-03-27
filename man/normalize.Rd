\name{normalize}
\alias{normalize}
\alias{normalize,data.frame-method}
\docType{methods}
\title{Normalize cell numbers}
\description{Normalize the relative or percental cell numbers.}
\usage{\S4method{normalize}{data.frame}(x,method="mean",show_avg=TRUE,\dots)}
\arguments{
\item{x}{Data frame of all relative or percental cell numbers. Use one row per sample and one column per gate. Use the first column for the first gate. 
If your first column is used e.g. for sample names read the manual before you continue. The names of the gates may not contain any whitespace. If the values contain commas 
they must be expressed as ".". Missing values or NA's are allowed but should be expressed as "NA" or empty entries within the table.}
\item{method}{Character string indicating which method is used for normalization (default=\emph{"mean"}). Can be changed to \emph{"first"}. See Details for further information.}
\item{show_avg}{logical (default=TRUE). Show average values. Can be changed to FALSE if the averages are not of interest. Averages are only shown if \bold{method}=\emph{"mean"}.}
\item{...}{Additional parameters used for printing and writing e.g. number of digits.}
}
\details{This step should be done to ensure the comparability of gates with highly different (low and high) cell abundances. There are two different methods to do this. 
By choosing \emph{"mean"} the individual cell number of each gate is normalized by dividing the relative value by the mean of the relative or percental cell numbers for that gate. 
By choosing \emph{"first"} the individual cell number of each gate is normalized by dividing the relative value by the first value of that gate.}
%\references{}
%\note{}
\author{Joachim Schumann \email{joachim.schumann@ufz.de}, Christin Koch \email{christin.koch@ufz.de}, Susanne Günther \email{susanne.günther@ufz.de}, Ingo Fetzer \email{info.fetzer@stockholmresilience.su.se}, Susann Müller \email{susann.müller@ufz.de}}
\examples{

## Normalize the percental cell numbers saved in dataset Cell_number_sample
## Use the default method and show the averages with two digits
## Note that the averages are very different but the normalized values
## are in the same range

data(Cell_number_sample)
normalize(Cell_number_sample[,-1],digits=2)

## Use the method "first"
## Print normalized values
## After normalization the first value of every gate is 1.00

data(Cell_number_sample)
normalize(Cell_number_sample[,-1],method="first",digits=2)
}

\keyword{normalize}
\keyword{file.show(file.path(R.home("doc"),"KEYWORDS"))}
