######
## Barcode of normalized values and boxplot of cell numbers
######

setMethod("cybar_plot", signature=c(x="data.frame",y="data.frame"),
definition = function(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",order="original",boxmain="",verbose=FALSE,...){
  
  newlist<-list() 
  
      lmat <- rbind( c(4,3,5), c(2,1,5))
      lhei <- c(1.5,4)
      lwid <- c(1.5,4,5)
      heatout<-heatmap.2(as.matrix(x),lmat=lmat,lhei=lhei,lwid=lwid,Rowv=Rowv,col=bluered(grad),trace=trace,tracecol=tracecol,dendrogram=dendrogram,
	  breaks=seq(from=from,to=to,length=grad+1),na.color=na.color,main=barmain,...) # create heatmap
      layout(lmat,heights=lhei,widths=lwid)
      par(new=TRUE) 
 
  cols<-as.numeric(heatout$colInd)  
  newlist$heat<-heatout
  
    if(labels=="hor"){
      if(order=="sim")
      boxout<-boxplot(y[cols],las=1,main=boxmain,...)
      else if (order=="original")
        boxout<-boxplot(y,las=1,main=boxmain,...)
         }
    else if(labels=="vert"){
      if(order=="sim")
        boxout<-boxplot(y[cols],las=2,main=boxmain,...)
      else if (order=="original")
        boxout<-boxplot(y,las=2,main=boxmain,...)
         }
    newlist$box=boxout
  
  if(verbose)
  return(newlist)  
  })

setMethod("cybar_plot", signature=c(x="matrix",y="matrix"),
          definition = function(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",order="original",boxmain="",verbose=FALSE,...){
            
            newlist<-list() 
            
            lmat <- rbind( c(4,3,5), c(2,1,5))
            lhei <- c(1.5,4)
            lwid <- c(1.5,4,5)
            heatout<-heatmap.2(as.matrix(x),lmat=lmat,lhei=lhei,lwid=lwid,Rowv=Rowv,col=bluered(grad),trace=trace,tracecol=tracecol,dendrogram=dendrogram,
                               breaks=seq(from=from,to=to,length=grad+1),na.color=na.color,main=barmain,...) # create heatmap
            layout(lmat,heights=lhei,widths=lwid)
            par(new=TRUE) 
            
            cols<-as.numeric(heatout$colInd)  
            newlist$heat<-heatout
            
            if(labels=="hor"){
              if(order=="sim")
                boxout<-boxplot(y[cols],las=1,main=boxmain,...)
              else if (order=="original")
                boxout<-boxplot(y,las=1,main=boxmain,...)
            }
            else if(labels=="vert"){
              if(order=="sim")
                boxout<-boxplot(y[cols],las=2,main=boxmain,...)
              else if (order=="original")
                boxout<-boxplot(y,las=2,main=boxmain,...)
            }
            newlist$box=boxout
            
            if(verbose)
              return(newlist)  
          })

setMethod("cybar_plot", signature=c(x="matrix",y="data.frame"),
          definition = function(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",order="original",boxmain="",verbose=FALSE,...){
            
            newlist<-list() 
            
            lmat <- rbind( c(4,3,5), c(2,1,5))
            lhei <- c(1.5,4)
            lwid <- c(1.5,4,5)
            heatout<-heatmap.2(as.matrix(x),lmat=lmat,lhei=lhei,lwid=lwid,Rowv=Rowv,col=bluered(grad),trace=trace,tracecol=tracecol,dendrogram=dendrogram,
                               breaks=seq(from=from,to=to,length=grad+1),na.color=na.color,main=barmain,...) # create heatmap
            layout(lmat,heights=lhei,widths=lwid)
            par(new=TRUE) 
            
            cols<-as.numeric(heatout$colInd)  
            newlist$heat<-heatout
            
            if(labels=="hor"){
              if(order=="sim")
                boxout<-boxplot(y[cols],las=1,main=boxmain,...)
              else if (order=="original")
                boxout<-boxplot(y,las=1,main=boxmain,...)
            }
            else if(labels=="vert"){
              if(order=="sim")
                boxout<-boxplot(y[cols],las=2,main=boxmain,...)
              else if (order=="original")
                boxout<-boxplot(y,las=2,main=boxmain,...)
            }
            newlist$box=boxout
            
            if(verbose)
              return(newlist)  
          })

setMethod("cybar_plot", signature=c(x="data.frame",y="matrix"),
          definition = function(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",labels="hor",order="original",boxmain="",verbose=FALSE,...){
            
            newlist<-list() 
            
            lmat <- rbind( c(4,3,5), c(2,1,5))
            lhei <- c(1.5,4)
            lwid <- c(1.5,4,5)
            heatout<-heatmap.2(as.matrix(x),lmat=lmat,lhei=lhei,lwid=lwid,Rowv=Rowv,col=bluered(grad),trace=trace,tracecol=tracecol,dendrogram=dendrogram,
                               breaks=seq(from=from,to=to,length=grad+1),na.color=na.color,main=barmain,...) # create heatmap
            layout(lmat,heights=lhei,widths=lwid)
            par(new=TRUE) 
            
            cols<-as.numeric(heatout$colInd)  
            newlist$heat<-heatout
            
            if(labels=="hor"){
              if(order=="sim")
                boxout<-boxplot(y[cols],las=1,main=boxmain,...)
              else if (order=="original")
                boxout<-boxplot(y,las=1,main=boxmain,...)
            }
            else if(labels=="vert"){
              if(order=="sim")
                boxout<-boxplot(y[cols],las=2,main=boxmain,...)
              else if (order=="original")
                boxout<-boxplot(y,las=2,main=boxmain,...)
            }
            newlist$box=boxout
            
            if(verbose)
              return(newlist)  
          })

setMethod("cybar_plot", signature=c(x="data.frame",y="missing"),
definition = function(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",verbose=FALSE,...){

heatout<-heatmap.2(as.matrix(x),Rowv=Rowv,col=bluered(grad),trace=trace,tracecol=tracecol,dendrogram=dendrogram,
			breaks=seq(from=from,to=to,length=grad+1),na.color=na.color,main=barmain,...) # create heatmap

			if(verbose)
				return(heatout)
})

setMethod("cybar_plot", signature=c(x="matrix",y="missing"),
          definition = function(x,y,Rowv=FALSE,grad=21,trace="none",tracecol="black",dendrogram="col",from=0,to=2,na.color="black",barmain="",verbose=FALSE,...){
            
            heatout<-heatmap.2(as.matrix(x),Rowv=Rowv,col=bluered(grad),trace=trace,tracecol=tracecol,dendrogram=dendrogram,
                               breaks=seq(from=from,to=to,length=grad+1),na.color=na.color,main=barmain,...) # create heatmap
            
            if(verbose)
              return(heatout)
          })


setMethod("cybar_plot", signature=c(x="missing",y="data.frame"),
definition = function(x,y,labels="hor",boxmain="",verbose=FALSE,...){

if(labels=="hor"){
        boxout<-boxplot(y,las=1,main=boxmain,...)
         }
    else if(labels=="vert"){
        boxout<-boxplot(y,las=2,main=boxmain,...)
         }
		 if(verbose)
		 return(boxout)
})

setMethod("cybar_plot", signature=c(x="missing",y="matrix"),
          definition = function(x,y,labels="hor",boxmain="",verbose=FALSE,...){
            
            if(labels=="hor"){
              boxout<-boxplot(y,las=1,main=boxmain,...)
            }
            else if(labels=="vert"){
              boxout<-boxplot(y,las=2,main=boxmain,...)
            }
            if(verbose)
              return(boxout)
          })