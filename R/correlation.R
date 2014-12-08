######
## Visualize correlation in a heatplot
######

setMethod("correlation", signature=(x="data.frame"),
definition = function (x,cortype="spearman",exact=FALSE,colkey=bluered(21),Rowv=TRUE,Colv=TRUE,symm=TRUE,
                         distfun=function(c) as.dist(1 - c),dendrogram="both",main="",verbose=FALSE,...) {
  newlist<-list()
  est <- function(a, b) cor.test(a, b,method=cortype,exact=exact)[["estimate"]]  # calculate estimates
  pval <- function(c, d) cor.test(c, d,method=cortype,exact=exact)[["p.value"]]  # calculate p-values
  
  e <- outer(colnames(x), colnames(x),Vectorize(function(i,j) est(x[,i], x[,j])))
  dimnames(e) <- list(colnames(x), colnames(x))
  newlist$est<-e
  
  p <- outer(colnames(x), colnames(x),Vectorize(function(k,l) pval(x[,k], x[,l])))
  dimnames(p) <- list(colnames(x), colnames(x))
  newlist$pvals<-p
    
  heatout<-heatmap.2(e, Rowv=Rowv,Colv=Colv, symm=symm, col=colkey, distfun=distfun,dendrogram=dendrogram,main=main,trace="none",...)  # create heatmap
  newlist$heat<-heatout
  
  if(verbose)
    return(newlist)
})

setMethod("correlation", signature=(x="matrix"),
          definition = function (x,cortype="spearman",exact=FALSE,colkey=bluered(21),Rowv=TRUE,Colv=TRUE,symm=TRUE,
                                 distfun=function(c) as.dist(1 - c),dendrogram="both",main="",verbose=FALSE,...) {
            newlist<-list()
            est <- function(a, b) cor.test(a, b,method=cortype,exact=exact)[["estimate"]]  # calculate estimates
            pval <- function(c, d) cor.test(c, d,method=cortype,exact=exact)[["p.value"]]  # calculate p-values
            
            e <- outer(colnames(x), colnames(x),Vectorize(function(i,j) est(x[,i], x[,j])))
            dimnames(e) <- list(colnames(x), colnames(x))
            newlist$est<-e
            
            p <- outer(colnames(x), colnames(x),Vectorize(function(k,l) pval(x[,k], x[,l])))
            dimnames(p) <- list(colnames(x), colnames(x))
            newlist$pvals<-p
            
            heatout<-heatmap.2(e, Rowv=Rowv,Colv=Colv, symm=symm, col=colkey, distfun=distfun,dendrogram=dendrogram,main=main,trace="none",...)  # create heatmap
            newlist$heat<-heatout
            
            if(verbose)
              return(newlist)
          })