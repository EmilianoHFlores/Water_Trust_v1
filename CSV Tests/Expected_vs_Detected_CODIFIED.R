library(tidyverse)
library(ggplot2)
library(readr)
library(googledrive)

nodeData <- read.csv("https://raw.githubusercontent.com/EmilianoHFlores/Water_Trust_v1/main/CSV%20Tests/Expected_vs_Detected_v1.csv")

node_characteristics = 4

dataLength = length(nodeData[,4])-node_characteristics
x <- seq(1:dataLength)

colExpected=4
colDetected=5

y1=nodeData[,colExpected]
y1=tail(y1,dataLength)
y2=nodeData[,colDetected]
y2=tail(y2,dataLength)

filename = paste(nodeData[1,2], "_", nodeData[2,colExpected], "_", nodeData[3,colExpected], ".png", sep="")

png(file=filename, width=600, height=350)


plot(x, y1, type = "l", ylim=c(0,100))
lines(x, y2, type = "l")

for (i in 1:(dataLength-1)){
  xmin=i;
  xmax=1+i;
  
  if ((y1[i])>(y2[i]+10)){
    polygon(c(x[x >= xmin & x <= xmax],
              rev(x[x >= xmin & x <= xmax])),
            c(y2[x >= xmin & x <= xmax],
              rev(y1[x >= xmin & x <= xmax])),
            col = "#D32500")
  }
  else{
    polygon(c(x[x >= xmin & x <= xmax],
              rev(x[x >= xmin & x <= xmax])),
            c(y2[x >= xmin & x <= xmax],
              rev(y1[x >= xmin & x <= xmax])),
            col = "#6BD7AF")
  }
  
}

lines(x, y1)
lines(x, y2)

dev.off()

drivefind <- drive_find(n_max = 30)

drive_upload("saving_plot1.png",
             path = "Water_Trust_Data",
             name = "saving_plot.png",
             overwrite = TRUE)
