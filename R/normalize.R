######
## Normalize cell abundances
######

setMethod("normalize", signature=(x="data.frame"),
definition = function(x,method="mean",show_avg=TRUE,...) {
  
  if(method=="mean"){
    normalize<-apply(x,1,function(y) y/colMeans(x,na.rm=TRUE))  # normalize the values
    normalized<-as.data.frame(t(normalize))
    if (show_avg)
      print (colMeans(x,na.rm=TRUE),...) # print averages
  }
  else if (method=="first"){
    normalize<-apply(x,1,function(y) y/as.numeric(head(x,n=1L)))  # normalize the values
    normalized<-as.data.frame(t(normalize))
  }  
  
  return(format(normalized,...))
})

setMethod("normalize", signature=(x="matrix"),
          definition = function(x,method="mean",show_avg=TRUE,...) {
            
            if(method=="mean"){
              normalize<-apply(x,1,function(y) y/colMeans(x,na.rm=TRUE))  # normalize the values
              normalized<-as.data.frame(t(normalize))
              if (show_avg)
                print (colMeans(x,na.rm=TRUE),...) # print averages
            }
            else if (method=="first"){
              normalize<-apply(x,1,function(y) y/as.numeric(head(x,n=1L)))  # normalize the values
              normalized<-as.data.frame(t(normalize))
            }  
            
            return(format(normalized,...))
          })