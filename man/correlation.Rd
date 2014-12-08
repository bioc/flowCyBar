\encoding{UTF-8}
\name{correlation}
\alias{correlation}
\alias{correlation,data.frame-method}
\alias{correlation,matrix-method}
\docType{methods}
\title{Visualize the correlation}
\description{A function to visualize the correlation of the relative or percental cell numbers and the abiotic parameters.}
\usage{
\S4method{correlation}{data.frame}(x,cortype="spearman",exact=FALSE,colkey=bluered(21),Rowv=TRUE,Colv=TRUE,symm=TRUE,distfun=function(c) as.dist(1 - c),dendrogram="both",main="",verbose=FALSE,\dots)

\S4method{correlation}{matrix}(x,cortype="spearman",exact=FALSE,colkey=bluered(21),Rowv=TRUE,Colv=TRUE,symm=TRUE,distfun=function(c) as.dist(1 - c),dendrogram="both",main="",verbose=FALSE,\dots)
}
\arguments{
\item{x}{Data frame or matrix of correlation values. Use one row per sample and one column per gate/abiotic parameter. Use the first column for the first gate/abiotic parameter. 
The names of the gates and abiotic parameters may not contain any whitespace. If the values contain commas they must be expressed as ".". 
Missing values or NA's are allowed but should be expressed as "NA" or empty entries within the table.}
\item{cortype}{Character string indicating which correlation coefficient is used. Choose between \emph{"spearman"} (default), \emph{"kendall"} and \emph{"pearson"}. See Details for
more information.}
\item{exact}{logical (default=FALSE). Do not compute exact p-values. Used for Kendall's tau and Spearman's rho. Can be changed to TRUE if R can compute exact p-values without ties. 
Computing exact p-values depends on the dataset. For more details type "?cor.test" into R command line.}
\item{colkey}{Indicating the color key/palette used for visualization (default=\emph{bluered(21)}). See Details for more information.}
\item{Rowv}{logical (default=TRUE). The row dendrogram will be arranged according to similarity. FALSE will arrange the dendrogram according to the sequence of data e.g. according to sampling.}
\item{Colv}{logical (default=TRUE). Determines how the column dendrogram should be reordered. Has the same options as the Rowv argument above.}
\item{symm}{logical (default=TRUE). Indicates if x should be treated symmetrically. Can only be TRUE if x is a square matrix.}
\item{distfun}{Function used for the computation of the distance (dissimilarity) between both rows and columns (default=function(c) as.dist(1 - c)).}
\item{dendrogram}{Character string indicating whether dendrograms should be displayed (default=\emph{"both"}). Can be changed to \emph{"row"}, \emph{"column"} or \emph{"none"}.
\bold{Rowv/Colv} has to be set TRUE to show a dendrogram for the rows/columns.}
\item{main}{Character string used as title of the correlation heatmap (default="")}
\item{verbose}{logical (default=FALSE). Change to TRUE to print estimates and additional information to the heatmap.}
\item{...}{Additional heatmap parameters. For more details type "?heatmap.2" into R command line.}
}
\details{Visualize the correlations between the cell number variations and abiotic parameters as a heatmap using the heatmap.2 function of the package "gplots" (see reference Warnes et al. 2013). 
Spearman's rank-order correlation coefficient (r_S) is used for this purpose, by default (see reference Koch et al. 2013, Box 1 for more details). This value varies between -1 and 1. 
The stronger the correlation, the closer the value of the coefficient to 1 (for positive correlations) or -1 (for negative correlations). 
Kendall's tau rank correlation coefficient also assigns the values to ranks but considers the difference between the probability that any two points will agree on 
the relative ranks compared to the probability that they will disagree. Pearson's product-moment correlation coefficient uses the true values and can be chosen to depict linear relationships.  
You can choose the color key/palette and the gradiations for visualization. By choosing the default color key positive correlations will be shown in shades of red, negative correlations 
will be shown in shades of blue and neutral correlations will be shown in white. Other possibilities are e.g. topo.colors(16), cm.colors(15) or heat.colors(11). 
The space within the brackets defines the gradiations of the color key.  For more details type "??Palettes" into R command line. 
It is also possible to define an own color palette. For more details on that type "?colorRampPalette" into R command line.
}
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

## Show the correlation saved in dataset Corr_data_sample
## Use default parameters

data(Corr_data_sample)
correlation(Corr_data_sample[,-1])

## Change the title to "Correlation heatmap"
## Use palette heat.colors(11) as color key

data(Corr_data)
correlation(Corr_data_sample[,-1],colkey=heat.colors(11),main="Correlation heatmap")

}

\keyword{correlation}
\keyword{file.show(file.path(R.home("doc"),"KEYWORDS"))}
