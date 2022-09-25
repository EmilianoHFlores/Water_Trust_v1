library(data.table)

# Theoretical data
alturaTinaco = 6.10
alturaAgua = 0.001
alturaSensor = 5.6
areaTinaco = 0.95
areaSistema = 0.000113
sensorID = 1

# Water speed and flow when it exits the tank
velocidadInicial = sqrt((2 * 9.81 * (alturaAgua)) / (1 - (areaSistema / areaTinaco)^2))
caudalILM = 60000 * velocidadInicial * areaSistema
caudalILH = caudalILM * 60

# Water speed and flow
velocidadFinal = sqrt(velocidadInicial^2 + 2 * 9.81 * (alturaTinaco - alturaSensor))
caudalFLM = 60000 * velocidadFinal * areaSistema
caudalFLH = caudalFLM * 60
vcaudalFLM = rnorm(length(frecuenciaSensor), mean = caudalFLM, sd=2)
vcaudalFLH = rnorm(length(frecuenciaSensor), mean = caudalFLH, sd=2)

# Expected values table
valoresEsperados = matrix(c(caudalFLM, caudalFLH), ncol = 2, byrow = TRUE)
colnames(valoresEsperados) = c('Flujo L/min', 'Flujo L/h')
rownames(valoresEsperados) = c('Valores esperados')
tabla = as.table(valoresEsperados)
tabla

# Collected data

# Import .csv file 
setwd('/Users/roberto/Documents/GitHub/Water_Trust_v1/Registro_Datos')
data <- read.csv("datosagua.csv", header = FALSE)
frecuenciaSensor = c()

for (i in 1:nrow(data)) {
  frecuenciaSensor[i] <- data[i, 1]
}

# Leaks detection
# Sensor
i=1
tiempoInicio = 0
tiempoMedicion = 1
tiempoInput = 10
tiempoFinal = 100
factorConversion = 7.5

tabla1 = data.table(FlujoLmin=c(numeric()), FlujoLhora=c(numeric()), FlujoLminEsperado=c(numeric()), FlujoLhoraEsperado=c(numeric()))


#frecuenciaSensor

caudalLM=0
caudalLH=0
# Caudal y gasto
x=0
while (i < length(frecuenciaSensor)) {
  if(frecuenciaSensor[i] > 0) {
    caudalLM = frecuenciaSensor[i]/factorConversion
    #print(caudalLM)
    caudalLH = caudalLM * 60
    #print(caudalLH)
    volGasto = caudalLM * 60 * (tiempoFinal - tiempoInicio)
    datos = data.table(FlujoLmin=c(caudalLM), FlujoLhora=c(caudalLH), FlujoLminEsperado=c(vcaudalFLM[i]), FlujoLhoraEsperado=c(vcaudalFLH[i]))
    tabla1 = rbind(tabla1, datos)
  }
  i=i+1
}

#tabla1

