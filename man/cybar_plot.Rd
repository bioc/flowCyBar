\name{cybar_plot}
\alias{cybar_plot}
\alias{cybar_plot,data.frame,data.frame-method}
\alias{cybar_plot,matrix,matrix-method}
\alias{cybar_plot,data.frame,matrix-method}
\alias{cybar_plot,matrix,data.frame-method}
\alias{cybar_plot,data.frame,missing-method}
\alias{cybar_plot,matrix,missing-method}
\alias{cybar_plot,missing,data.frame-method}
\alias{cybar_plot,missing,matrix-method}

\docType{methods}
\title{Barcode of the normalized cell numbers and boxplot of relative or percental cell numbers}
\description{Show a barcode of the variation of normalized cell numbers (CyBar plot) and a boxplot of the relative or percental cell numbers.}
\usage{

\S4method{cybar_plot}{data.frame,data.frame}(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",
order="original",boxmain="",verbose=FALSE,\dots)

\S4method{cybar_plot}{matrix,matrix}(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",
dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",
order="original",boxmain="",verbose=FALSE,\dots)

\S4method{cybar_plot}{data.frame,matrix}(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",
order="original",boxmain="",verbose=FALSE,\dots)

\S4method{cybar_plot}{matrix,data.frame}(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",
order="original",boxmain="",verbose=FALSE,\dots)

\S4method{cybar_plot}{data.frame,missing}(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",
dendrogram="col",from=0,to=2,na.color="black",barmain="",verbose=FALSE,\dots)

\S4method{cybar_plot}{matrix,missing}(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",
dendrogram="col",from=0,to=2,na.color="black",barmain="",verbose=FALSE,\dots)

\S4method{cybar_plot}{missing,data.frame}(x,y,labels="hor",boxmain="",verbose=FALSE,\dots)

\S4method{cybar_plot}{missing,matrix}(x,y,labels="hor",boxmain="",verbose=FALSE,\dots)
}
\arguments{
\item{x}{Data frame or matrix of normalized cell numbers. Use one row per sample and one column per gate. Use the first column for the first gate. 
The names of the gates may not contain any whitespace. If the values contain commas they must be expressed as ".". Missing values or NA's are allowed but should be 
expressed as "NA" or empty entries within the table.}
\item{y}{Data frame or matrix of relative or percental cell numbers. Same requirements as described for \emph{x}.}
\item{Rowv}{Keeps sequence of data in the barcode e.g. according to sampling (default=FALSE). If TRUE it will be arranged according to similarity.}
\item{grad}{Gradiations of the color key used in the barcode (default=21).}
\item{trace}{Display trace lines in the barcode (default=\emph{"none"}). For displaying trace lines choose between \emph{"column"}, \emph{"row"} or \emph{"both"}.}
\item{tracecol}{Color of traces in the barcode if trace is active (default=\emph{"black"}).}
\item{dendrogram}{Character string indicating whether dendrograms should be displayed in the barcode (default=\emph{"column"}). Can be changed to \emph{"row"}, \emph{"both"} or 
\emph{"none"}. If you want to display a dendrogram for the rows and columns change to \emph{"both"} and \bold{Rowv} to TRUE.}
\item{from}{Beginning of the data range of the normalized cell numbers (default=0).}
\item{to}{End of the data range of the normalized cell numbers (default=2).}
\item{na.color}{Color used for missing values or NA (default=\emph{"black"}).}
\item{barmain}{Character string used as title of the barcode (default="").}
\item{labels}{Character string indicating whether horizontal (\emph{"hor"}) data labels (default) or vertical (\emph{"vert"}) data labels should be plotted in the boxplot.}
\item{order}{Character string indicating the order of the gates in the boxplot (default=\emph{"original"}). The boxes are ordered respective to the original order of the gates. 
Choose \emph{"sim"} if the boxes should be ordered respective to the similarity calculated in the heatmap function. This can only be done if x is given and the barcode is created.}
\item{boxmain}{Character string used as title of the boxplot (default="").}
\item{verbose}{logical (default=FALSE). Change to TRUE to print additional information to the barcode and/or the boxplot.}
\item{...}{Additional plotting parameters used for the heatmap and the boxplot. For more details type "?heatmap.2" or "?boxplot" into R command line.}
}
\details{This function generates a barcode of the normalized cell numbers for all measurements using the heatmap.2 function of the package "gplots" (see reference Warnes et al. 2013). 
The barcode displays the variation for each gate using a color key with 21 gradations. Very low values are shown in blue, intermediate values are shown in white and high values are sown in deep red. 
Missing values or NA's are shown in black. In addition a dendrogram to reveal clusters and a histogram of the values are shown. The distribution of the relative or percental cell numbers per gate is visualized as a 
vertical boxplot. One box consists of the median, the upper and lower quartil (top and bottom of the box) containing 50\% of the data, whiskers showing the minimum 
(within 1.5-fold of the interquartile range of the lower quartile) and the maximum (within 1.5-fold of the interquartile range of the upper quartile) of the data and outliers. Choose between 
horizontal ("hor") and vertical ("vert") data labels that represent the gates. See reference Koch et al. 2013 for more details.}

\references{\emph{Koch, Christin and Günther, Susanne and Desta, Adey Feleke and Hübschmann, Thomas and Müller, Susann (2013). 
Cytometric fingerprinting for analysing microbial intra-community structure variation and identifying sub-community function. 
Nature Protocols, 8(1):190-202},

\emph{Gregory R. Warnes, Ben Bolker, Lodewijk Bonebakker, Robert Gentleman, Wolfgang Huber Andy
  Liaw, Thomas Lumley, Martin Maechler, Arni Magnusson, Steffen Moeller, Marc Schwartz and Bill
  Venables (2013). gplots: Various R programming tools for plotting data. R package version
  2.12.1. http://CRAN.R-project.org/package=gplots }}
%\note{}
\author{Joachim Schumann \email{joachim.schumann@ufz.de}, Christin Koch \email{christin.koch@ufz.de}, Susanne Günther \email{susanne.guenther@ufz.de}, Ingo Fetzer \email{info.fetzer@stockholmresilience.su.se}, Susann Müller \email{susann.mueller@ufz.de}}
\examples{require(gplots)

## Show a barcode of the normalized cell numbers
## and a boxplot with horizontal data labels of the percental cell numbers
## saved in dataset Cell_number_sample
## The boxes are ordered according to the original order

data(Cell_number_sample)
Normalized_mean<-normalize(Cell_number_sample[,-1],digits=2)
Normalized_mean<-data.frame(data.matrix(Normalized_mean))
cybar_plot(Normalized_mean,Cell_number_sample[,-1])

## Barcode and boxplot are titled according to the dataset names, respectively
## The orientation of the data labels is vertical

data(Cell_number_sample)
Normalized_mean<-normalize(Cell_number_sample[,-1],digits=2)
Normalized_mean<-data.frame(data.matrix(Normalized_mean))
cybar_plot(Normalized_mean,Cell_number_sample[,-1],
barmain="Barcode of normalized cell numbers",labels="vert",boxmain="Boxplot of cell numbers")

## Show a barcode of the normalized cell numbers using method "first"
## The end of the data range of the normalized cell numbers is set to 3.5
## The boxes are ordered according to the similarity calculated in the barcode

data(Cell_number_sample)
Normalized_first<-normalize(Cell_number_sample[,-1],method="first",digits=2)
Normalized_first<-data.frame(data.matrix(Normalized_first))
cybar_plot(Normalized_first,Cell_number_sample[,-1],to=3.5,
barmain="Barcode of normalized cell numbers",labels="vert",order="sim",boxmain="Boxplot of cell numbers")

}

\keyword{barcode,normalize}
\keyword{file.show(file.path(R.home("doc"),"KEYWORDS"))}
