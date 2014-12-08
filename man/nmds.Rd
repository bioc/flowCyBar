\name{nmds}
\alias{nmds}
\alias{nmds,data.frame-method}
\alias{nmds,matrix-method}
\docType{methods}
\title{NMDS plot of cell numbers}
\description{NMDS plot of the relative/percental or normalized cell numbers.}
\usage{
\S4method{nmds}{data.frame}(x,distance="bray",autotransform=FALSE,zerodist="add",group,main="",type="p",
cex=0.6,pos=4,shrink=TRUE,legend_pos="topleft",pch=1,col="black",abiotic,p.max=0.05,col_abiotic="magenta",verbose=FALSE,\dots)

\S4method{nmds}{matrix}(x,distance="bray",autotransform=FALSE,zerodist="add",group,main="",type="p",
cex=0.6,pos=4,shrink=TRUE,legend_pos="topleft",pch=1,col="black",abiotic,p.max=0.05,col_abiotic="magenta",verbose=FALSE,\dots)
}
\arguments{
\item{x}{Data frame or matrix of relative/percental cell numbers or normalized cell numbers. Use one row per sample and one column per gate. Use the first column for the first gate.
 The names of the gates may not contain any whitespace. If the values contain commas they must be expressed as ".". Missing values or NA's are not allowed.}
\item{distance}{Dissimilarity index used in vegdist (default=\emph{"bray"}). For more details type "?metaMDS" into R command line.}
\item{autotransform}{logical (default=FALSE). Determine the use of simple heuristics for a possible data transformation of typical community data. For more details type 
"?metaMDS" into R command line.}
\item{zerodist}{Handling of zero dissimilarities (default=\emph{"add"}). For more details type "?metaMDS" into R command line.)}
\item{group}{Table with group assignments. Use only one column. Use the first line as header. Assign the samples to groups in the next lines.
 The order and the number of these lines has to be identical to the order and the number of the samples printed in R. Use only integer values in the range from 1 to 25.}
\item{main}{Character string used as title of the NMDS plot (default="")}
\item{type}{Type of the plot (default=\emph{"p"}). The "p" indicates points. For more details type "?points" into R command line.}
\item{cex}{numeric (default=0.6). Character expansion factor. Used for the final size of the characters.}
\item{pos}{Position of the text (default=4). Values of 1, 2, 3 and 4 indicate positions below, to the left of, above and to the right of the specified coordinates, respectively.}
\item{shrink}{logical (default=TRUE). Shrink back species scores if they were expanded originally.}
\item{legend_pos}{Position of the legend (default=\emph{"topleft"}). For more details type "?legend" into R command line.}
\item{pch}{Plotting symbol (default=1) if \bold{group} is FALSE. For more details type "?points" into R command line.}
\item{col}{Color of the symbols and lines within the plot (default=\emph{"black"}) if \bold{group} is FALSE . Can be a color code or a name.}
\item{abiotic}{Table with abiotic data. Should be a tab-delimited text file using '.' as decimal delimiter. 
Use one row for one sample and one column for one abiotic parameter. Use the first column for the first parameter and the first line as header.
The order and the number of the lines have to be identical to the order and the number of the samples printed in R.}
\item{p.max}{Decimal number defining the significance level of the abiotic parameters (default=0.05) if \bold{abiotic}=TRUE. Only parameters less/equal this value are plotted.}
\item{col_abiotic}{Color used for the plotted abiotic parameters (default=\emph{"magenta"}).}
\item{verbose}{logical (default=FALSE). Change to TRUE to print results of the metaMDS function and the p-values of the abiotic parameters.}
\item{...}{Additional parameters used for the plot and the metaMDS function. For more details type "?plot", "?points", "?text" or "?metaMDS" into R command line.}
}
\details{Visualizes the distance between samples based on their cytometrically measured cell abundance information using nonmetric multidimensional scaling (NMDS). 
NMDS is a mathematical technique to visualize the distances between objects (the samples, in this case). The distances within the plot are used to be 
in accordance with the dis-/similarities of the samples. The NMDS plot can be created using either the relative/percental cell numbers or the normalized values. 
If there are high abundance and low abundance gates the NMDS plot could be distorted. In this case it is better to use the normalized values. See reference Koch et al. 2013, Box 1 
for more details. The NMDS analysis is performed by using the metaMDS function of the R package "vegan" (see reference Warnes et al. 2013).}

\references{\emph{Koch, Christin and Günther, Susanne and Desta, Adey Feleke and Hübschmann, Thomas and Müller, Susann (2013). 
Cytometric fingerprinting for analysing microbial intra-community structure variation and identifying sub-community function. 
Nature Protocols, 8(1):190-202},

\emph{Gregory R. Warnes, Ben Bolker, Lodewijk Bonebakker, Robert Gentleman, Wolfgang Huber Andy
  Liaw, Thomas Lumley, Martin Maechler, Arni Magnusson, Steffen Moeller, Marc Schwartz and Bill
  Venables (2013). gplots: Various R programming tools for plotting data. R package version
  2.12.1. http://CRAN.R-project.org/package=gplots }}

%\note{}
\author{Joachim Schumann \email{joachim.schumann@ufz.de}, Christin Koch \email{christin.koch@ufz.de}, Susanne Günther \email{susanne.guenther@ufz.de}, Ingo Fetzer \email{info.fetzer@stockholmresilience.su.se}, Susann Müller \email{susann.mueller@ufz.de}}
\examples{require(vegan)


## Show the NMDS plot of the normalized cell numbers

data(Cell_number_sample)
Normalized_mean<-normalize(Cell_number_sample[,-1],digits=2)
Normalized_mean<-data.frame(data.matrix(Normalized_mean))
nmds(Normalized_mean)

## Change the title of the plot to "NMDS normalized", use triangles as plotting 
## symbols, change the color to red and position the text below the triangles
## Use a dotted line (lty=3)

data(Cell_number_sample)
Normalized_mean<-normalize(Cell_number_sample[,-1],digits=2)
Normalized_mean<-data.frame(data.matrix(Normalized_mean))
nmds(Normalized_mean,main="NMDS normalized",type="b",pos=1,pch=2,col="red",lty=3)

## Plot sample groups saved as data frame groups
## Print additional information

data(Cell_number_sample)
Normalized_mean<-normalize(Cell_number_sample[,-1],digits=2)
Normalized_mean<-data.frame(data.matrix(Normalized_mean))
groups<-data.frame("groups"=c(1,1,1,1,2,3,3,3,3,3))
nmds(Normalized_mean,group=groups,main="NMDS normalized",verbose=TRUE)

## Plot additional gate information and
## abiotic parameters saved in dataset Abiotic_data_sample

data(Cell_number_sample)
Normalized_mean<-normalize(Cell_number_sample[,-1],digits=2)
Normalized_mean<-data.frame(data.matrix(Normalized_mean))
groups<-data.frame("groups"=c(1,1,1,1,2,3,3,3,3,3))
data(Abiotic_data_sample)
nmds(Normalized_mean,group=groups,main="NMDS normalized",
abiotic=Abiotic_data_sample[,-1],verbose=TRUE)

}
\keyword{nmds,normalize}
\keyword{file.show(file.path(R.home("doc"),"KEYWORDS"))}