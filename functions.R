#### Basic Stats ####
## convert r to Cohen's D
r2d = function(r){
  d = sqrt((4*(r^2))/(1-r^2))
  return(d)
}

##extracts standard deviation table for DV (either 1 variable or a vector of variables)--for more information on how tapply works, use the R help or RStudio help menu
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Block)
sd.function = function(data, DV, IV){
	sd=with(data, tapply(DV, IV, sd))
	return(sd)
}

sd.function.na = function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  return(sd)
}


##extracts standard error table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to er
##  eg. er=se.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
se.function=function(data, DV, IV){
	sd=with(data, tapply(DV, IV, sd))
	length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
	er=sd/sqrt(length)
	return(er)
}

se.function.na=function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
  er=sd/sqrt(length)
  return(er)
}

##extracts mean table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to means
##  eg. means=means.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
means.function = function(data, DV, IV){
	means=with(data, tapply(DV, IV, mean))
	return(means)
}

means.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, mean, na.rm=T))
  return(means)
}

##extracts median table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to medians
##  eg. medians=med.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
med.function = function(data, DV, IV){
  means=with(data, tapply(DV, IV, median))
  return(means)
}

med.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, median, na.rm = T))
  return(means)
}

##extracts IQR table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to interquartile range
##  eg. IQR=IQR.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
IQR.function = function(data, DV, IV){
  means=with(data, tapply(DV, IV, IQR))
  return(means)
}

IQR.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, IQR, na.rm = T))
  return(means)
}

##extracts range table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to interquartile range
##  eg. IQR=IQR.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
range.function = function(data, DV, IV){
  ranges=with(data, tapply(DV, IV, range))
  return(ranges)
}

range.function.na = function(data, DV, IV){
  ranges=with(data, tapply(DV, IV, range, na.rm=T))
  return(ranges)
}

##extracts 95% confidence intervals for on DV overall or for levels of a grouping variable based on the t distribution (smaller sample sizes--if over 100 obs than need to use Z)
##IV=0, levelnum=0 means want overall CI for DV
##IV=factor, levelnum=0 means want CI for DV for each level of factor separately
##IV=factor, levelnum=# means want CI for DV for specific level of factor
##  --to see factor level number: levels(factorname)
CI_95.function = function(data, DV, IV, levelnum){

  if(length(IV)==1){
    stat=qt(0.975,df=(nrow(data)-1))
    error=(sd(DV)/sqrt(length(DV)))*stat
    right=as.numeric(mean(DV))+error
    left=as.numeric(mean(DV))-error
    CI_stat=concatonate_2col(round(left, 3),round(right, 3), "CI")
    output=CI_stat[1]
  }
  else {
    se=se.function(data, DV, IV)
    mean=means.function(data,DV,IV)
    if(levelnum==0){
      nlevel=length(levels(IV))
      levelname=c(levels(IV))
      CI=rep(0, nlevel)
      output=rbind(levelname, CI)
      for(i in 1:nlevel){
        level_data=data[which(IV==levels(IV)[i]), ]
        stat=qt(0.975,df=(nrow(level_data)-1))
        error=as.numeric(se)[i]*stat
        right=as.numeric(mean)[i]+error
        left=as.numeric(mean)[i]-error
        CI_stat=concatonate_2col(round(left, 3),round(right, 3), CI)
        output[2,i]=CI_stat[1]
      }
    }
    else {
      level_data=data[which(IV==levels(IV)[levelnum]), ]

      stat=qt(0.975,df=(nrow(level_data)-1))
      error=as.numeric(se)[levelnum]*stat
      right=as.numeric(mean)[levelnum]+error
      left=as.numeric(mean)[levelnum]-error
      CI_stat=concatonate_2col(round(left,3),round(right,3), CI)
      output=CI_stat[1]
    }
  }
return(output)
}

#produces a matrix of pvalues from two-sided t-test (no correction, then bonferoni correct)

pvalue.matrix_none=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  p_num=((l-1)/2)

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      res=t.test(x,y,alternative='two.sided')$p.value

      p_res=res
      p_res=round(p_res, digits=3)

      if (p_res>0.150){
        p_res="NA"
      }
      if (p_res<0.001){
        p_res="<0.001"
      }

      res_matrix[icount,jcount]=p_res
    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

pvalue.matrix_bon=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  p_num=((l-1)/2)

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      res=t.test(x,y,alternative='two.sided')$p.value

      p_res=p.adjust(res, method = "bonferroni", n = (p_num))
      p_res=round(p_res, digits=3)

      if (p_res>0.150){
        p_res="NA"
      }
      if (p_res<0.001){
        p_res="<0.001"
      }

      res_matrix[icount,jcount]=p_res
    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_0.10=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]

      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)

      if (p_res>0.050){
        c_res="NA"
      }

      res_matrix[icount,jcount]=c_res

    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_0.05=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]

      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)

      if (p_res>0.050){
        c_res="NA"
      }

      res_matrix[icount,jcount]=c_res

    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}


##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_Method_0.05=function(var_vector, var_names, method){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]

      p_res=round(cor.test(x,y, na.rm=TRUE, method = method)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE, method = method)$estimate, 2)

      if (p_res>0.050){
        c_res="NA"
      }

      res_matrix[icount,jcount]=c_res

    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]

      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)

      if (p_res <= 0.05){
        res_matrix[icount,jcount]=paste0(c_res, '*')
      } else{
        res_matrix[icount,jcount]=c_res
      }


    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_ps=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]

      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)

      res_matrix[icount,jcount]=p_res

    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_Method=function(var_vector, var_names, method){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0

  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]

      p_res=round(cor.test(x,y, na.rm=TRUE, method=method)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE, method=method)$estimate, 2)

      res_matrix[icount,jcount]=c_res

    }

  }

  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- ""
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}



#### Functions for specific data #####

#check all conditions for SST racehorse model
racehorse_check_fn <- function(data, id){
  id_data <- data[data$sub == id,]
  ncond <- sum(id_data$racehorse_check == 1)
  return(ncond)
}

# get SST condition orders
order_fn <- function(data, sub, cond){
  sub_order <- data[data[['sub']] == sub, 'condition']

  if (cond == 'ED'){
    ED_order <- as.numeric(grepl('hED', sub_order, fixed = TRUE))
    ED_order <- paste(ifelse(ED_order == 1, 'hED', 'lED'), collapse = '_')
    return(ED_order)
  }

  if (cond == 'PS') {
    PS_order <- as.numeric(grepl('lPort', sub_order, fixed = TRUE))
    PS_order <- paste(ifelse(PS_order == 1, 'lPS', 'sPS'), collapse = '_')
    return(PS_order)
  }

  if (cond == 'all'){
    order <- paste(sub_order, collapse = '_')
    return(order)
  }

}

#### ANOVA Table -> data.frame() ####

anova2dataframe=function(output){
  output_table=data.frame(output)
  table_ncol <- ncol(output_table)
  output_table$sig <- ifelse(output_table[ncol(output_table)] > 0.10 , NA,
                             ifelse(output_table[ncol(output_table)] > 0.05, '.',
                                    ifelse(output_table[ncol(output_table)] > 0.01, '*',
                                           ifelse(output_table[ncol(output_table)] > 0.001, '**', '***'))))
  output_table[1:table_ncol] <- round(output_table[1:table_ncol], 3)
  return(output_table)
}
