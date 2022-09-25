library(tidyverse)
library(ggplot2)
library(readr)
library(googledrive)

nodeData <- read.csv("https://raw.githubusercontent.com/EmilianoHFlores/Water_Trust_v1/main/CSV%20Tests/Expected_vs_Detected.csv?token=GHSAT0AAAAAABZFJPKSGFOJKFPOP5GHG4X2YZPMWEA")

x <- seq(1:200)

y1=nodeData$EXPECTED
y2=nodeData$DETECTED

png(file="saving_plot1.png", width=600, height=350)


plot(x, y1, type = "l", ylim=c(0,100))
lines(x, y2, type = "l")

# l <- length(time)
# color <- hcl.colors(l, "TealGrn") # Palette
# 
# for (i in 1:l) {
#   polygon(c(time[i], rev(time[i])),
#           c(y2[i], rev(y1[i])),
#           border = color[i], col = NA)
# }

# Min and max X values

for (i in 1:(length(time)-1)){
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


