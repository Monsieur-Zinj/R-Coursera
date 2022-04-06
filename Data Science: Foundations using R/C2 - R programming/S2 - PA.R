library(comprehenr)

# create a list of dataframe depending on id provided
make_list_data <- function(directory, id){
  path="~/Documents/R-Coursera/R-Coursera/Data Science: Foundations using R/C2 - R programming/"
  path=paste(path,directory,sep="")

  id_char<-paste(id) # num to char

  zeros<-to_vec(for (i in nchar(id_char)) paste(rep("0",3-i),collapse = "")) # calculate how much zeros need to be padded before file name 
  
  file_list <- paste0(path,"/", zeros,id_char,".csv") #concatenate path,zeros, number and .csv
  
  myfiles = lapply(file_list, read.csv) # read all file and put data in a list
  
  myfiles

}



pollutantmean <- function(directory, polluant, id=1:332){
  

  # return sum of variables and number of no na
  mean_and_pond <- function(file){
    
      var_sum <- sum(file[,polluant], na.rm=TRUE)
      no_na_nb <- sum(!is.na(file[,polluant]))
      c(var_sum, no_na_nb)
  }
  
  myfiles<-make_list_data(directory,id)

  
  mean_and_pond_res<-lapply(myfiles,mean_and_pond)
  mean_and_pond_res<-rowSums(matrix(unlist(mean_and_pond_res),nrow = 2)) # add all sum and no na nb
  mean_res<-mean_and_pond_res[1]/mean_and_pond_res[2]
  mean_res
  
  
}


complete <- function(directory, id=1:332){
  
  myfiles<-make_list_data(directory,id)
  
  complete_one_dataframe <- function(file){
  
      complete_nb<-sum(rowSums(!is.na(file[,c(2,3)]), na.rm = TRUE)==2)  
      complete_nb  
  }
  
  complete_vect<- unlist(lapply(myfiles,complete_one_dataframe))
  
  var<-list(id,complete_vect)
  var<-data.frame(var)
  colnames(var)<-c("id","nobs")
  var
}
  
  
  
  





