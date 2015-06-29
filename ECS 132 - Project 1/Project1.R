data <-read.csv(file="Traffic_data_orig.csv",header=TRUE,sep=",")



string2binary = function(string, noBits) {
  number = 0
  number = as.numeric(charToRaw(string))
  
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

words <- strsplit("this is a string of words", "")[[1]]
numwords = length(words)


binArray = NULL


for (i in 1:numwords){

  binArray <- c(binArray,string2binary(words[i], 8))
  
}
binArray

stream = 0
listPackets = NULL

for (i in 1:length(binArray)){
  
  if (binArray[i] == 0){
    stream <-stream + .25
    
  }
  else{
    stream <- stream + .75
  }
  listPackets <- c(listPackets,stream)
}
listPackets


    
  


