#Programa Principal, para leer la base de datos en Github, construir las gráficas,
#subirlas a Drive y actualizar el directorio de enlaces.

library(tidyverse)
library(ggplot2)
library(readr)
library(googledrive)
library(googlesheets4)

nodeData <- read.csv("https://github.com/EmilianoHFlores/Water_Trust_v1/raw/main/CSV%20Tests/Expected_vs_Detected_v1.csv")

node_characteristics = 4
node_quantity = as.integer(nodeData[2,1])
device_quantity = as.integer(nodeData[3,1])

errorRange=0.1 #Percentage error to mark in red

links <- c()
imgNames <- c()

for(col in 0:(device_quantity-1)){
  
  #Localización de la columna según el diseño de la base de datos
  colExpected=4+2*col
  colDetected=colExpected+1
  
  #Creación del eje x según la cantidad de lecturas
  dataLength = length(nodeData[,colExpected])-node_characteristics
  x <- seq(1:dataLength)
  
  y1=nodeData[,colExpected]
  y1=tail(y1,dataLength)
  y2=nodeData[,colDetected]
  y2=tail(y2,dataLength)
  
  #Creación del nombre de archivo
  imgName = paste(nodeData[1,1], "_", nodeData[2,colExpected], "_", nodeData[3,colDetected], sep="")
  filename = paste(imgName, ".png", sep="")
  
  imgNames[col+1]=imgName
  
  png(file=filename, width=1000, height=600)
  
  highestValue = max(y1)
  
  #Gráfica de lineas
  plot(x, y1, type = "l", ylim=c(0,highestValue+highestValue*0.2))
  lines(x, y2, type = "l")
  
  #Sombreado de espacio entre líneas
  for (i in 1:(dataLength-1)){
    xmin=i;
    xmax=1+i;
    
    if ((y1[i])>(y2[i]+y1[i]*errorRange)){
      polygon(c(x[x >= xmin & x <= xmax],
                rev(x[x >= xmin & x <= xmax])),
              c(y2[x >= xmin & x <= xmax],
                rev(y1[x >= xmin & x <= xmax])),
              col = "#D32500",
              border = "transparent")
    }
    else{
      #Color alternativo
      polygon(c(x[x >= xmin & x <= xmax],
                rev(x[x >= xmin & x <= xmax])),
              c(y2[x >= xmin & x <= xmax],
                rev(y1[x >= xmin & x <= xmax])),
              col = "#6BD7AF",
              border = "transparent")
    }
    
    
  }
  
  lines(x, y1)
  lines(x, y2)
  
  dev.off()
  
  #Subida a drive
  driveUpload <- drive_upload(filename,
               path = "Water_Trust_Data",
               name = filename,
               overwrite = TRUE)
  
  driveUpload <- driveUpload %>%
      drive_share(role = "reader", type = "anyone")
  
  #Construcción del link de drive con su ID
  link = paste("https://drive.google.com/file/d/", driveUpload$id, sep="")
  links[col+1] = link

}

#Dataframe para subir los nombres de archivos y su link de drive
SSData = data.frame(
  Names = imgNames,
  Links = links
)

sheet_write(SSData, ss = "https://docs.google.com/spreadsheets/d/1Z3WUGTf0QGkb33F9MuMby8yLkf__k5JNziSedkim_AA/edit?usp=sharing", sheet = "Image_Links")

