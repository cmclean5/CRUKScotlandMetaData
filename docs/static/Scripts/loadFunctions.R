
duplicate.rows <- function(x,encoded=NULL){

  if( !is.null(x) && !is.null(encoded)){
    setDT(x)
    cnames    = colnames(x)
    col.indx  = grepl(encoded, cnames)
  
    x[, tmp := rowSums(.SD), .SDcols = col.indx]
    
    x=x[rep(1:.N,tmp)]
    
    ii = x[,.I[tmp>1]]
    jj = seq(1,dim(x)[2],1)
    jj = jj[grepl(encoded, colnames(x))]
    
    k=1
    for(i in 1:length(ii)){
      if( k < length(ii) ){
      start=ii[k]
      N=x[start,tmp]
      end=ii[k+N-1]
      Z=x[start:end,.SD,.SDcols=jj]
      Z=colSums(Z)>0
      zz = matrix(0,length(jj),length(jj))
      diag(zz) = 1
      for( j in 1:length(jj) ){
        if( Z[j]==FALSE ){ zz[,j]=0; }
      }
      zz=zz[rowSums(zz>0)==1,]
      x[start:end, c(jj):=as.data.table(zz)]
      k=k+N
    }
    }
    
    x[,tmp:=NULL]
        
  }
  
  return(x)
  
}


myValueBox <- function(title, caption="", color="cornflowerblue", myicon="", fontsize="25px"){
## Ref: https://stackoverflow.com/questions/71873849/loop-valueboxes-in-r-flexdashboards
    div(
    class = "value-box level3",
    style = glue::glue(
      '
            background-color: @{color}@;
            height: 106px;
            width: 18%;
            display: inline-block;
            overflow: hidden;
            word-break: keep-all;
            text-overflow: ellipsis;
            ', .open = '@{', .close = '}@'
    ),
    div(
      class = "inner",
      p(class = "value", title, style = glue::glue("font-size:{fontsize}")),
      p(class = "caption", caption)
    ),
    div(class = "icon", myicon)
  )
}