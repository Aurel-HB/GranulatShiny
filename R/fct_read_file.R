#' read_file 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

read_file<-function(file){
  file_lines<-readLines(file, warn = F)
  
  data<-as.data.frame(setNames(replicate(length(strsplit(file_lines[1], ";")[[1]]),numeric(0), simplify = F),strsplit(file_lines[1], ";")[[1]] ))
  data<-data[,-73]
  for(i in 2:length(file_lines)){
    data[i-1,]<-strsplit(file_lines[i], ";")[[1]]
  }
  data
}
