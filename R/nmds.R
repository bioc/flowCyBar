######
## NMDS plot of cell numbers / normalized cell numbers
######

setMethod("nmds", signature=(x="data.frame"),
definition = function(x,distance="bray",autotransform=FALSE,zerodist="add",group,main="",type="p",cex=0.6,pos=4,shrink=TRUE,legend_pos="topleft",pch=1,col="black",abiotic,p.max=0.05,col_abiotic="magenta",verbose=FALSE,...) {
  newlist<-list()
  mds.out <- metaMDS(x, distance=distance, autotransform=autotransform, zerodist=zerodist,...)    # performs nonmetric multidimensional scaling
  print(rownames(mds.out$points))
  newlist$mds.out<-mds.out
  plot(mds.out, type="n",main=main) # plot results
  text(mds.out,cex=cex,pos=pos,shrink=shrink,...)    # data label as text  
  
  if(missing(group))  # check for group file
    points(mds.out, type=type,pch=pch,col=col,...)  # add points  
    
  else{
    color<-group[,1]
    points(mds.out,type=type, col=color , pch=as.numeric(color))
    legend(x=legend_pos, legend=unique(color),  pch=as.numeric(unique(color)), col=as.numeric(unique(color)))
    ordihull(mds.out, groups=color, lty=2, col="darkgrey")   
    }
  
  if(!missing(abiotic)){  # check for abiotic file
    ef<-envfit(mds.out, abiotic, permutation=999)     # calculates the relevant environmental parameters for the nMDS #result based on 999 Monte-Carlo permutations
    plot (ef,cex=cex,p.max=p.max, col=col_abiotic)       # plots the most relevant environmental parameters (with a p.max <=0.05) into the MDS
    newlist$ef<-ef    
  }
  
  if(verbose)
  return(newlist)
  
})