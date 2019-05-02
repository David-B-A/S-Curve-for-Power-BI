source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
####################################################

################### Actual code ####################
############################################
dataset = data.frame(Value)
############################################
dataset$FECHA = as.Date(dataset$SEMANA)
RealPositions = which(dataset$REAL != "")

if(range(dataset$PLAN)[2]<=1){
  dataset$PLAN = dataset$PLAN*100
}
if(range(dataset$REAL[RealPositions])[2]<=1){
  dataset$REAL = dataset$REAL*100
}
somos =which(dataset$SOMO != "")
somos=c(somos,length(dataset$FECHA))

Dates = as.Date(dataset$FECHA[1:somos[1]])
Avance = dataset$PLAN[1:somos[1]]
Linea = c()
for(i in 1:somos[1]){
  Linea = c(Linea, "PLAN")
}

for(j in 1:(length(somos)-1)){
  Dates= c(Dates, dataset$FECHA[(somos[j]+1):somos[j+1]])
  Avance = c(Avance, dataset$PLAN[(somos[j]+1):somos[j+1]])
  for(k in (somos[j]+1):(somos[j+1])){
    Linea = c(Linea, paste("SOMO",j))
  }
}

Dates= c(Dates,dataset$FECHA[RealPositions])
Avance = c(Avance, dataset$REAL[RealPositions])
for(z in RealPositions){
  Linea = c(Linea, paste("REAL",j))
}


df <- data.frame(Dates = Dates,
                 Variable_Data=Avance, Variable_Name=Linea)
df$Dates <- as.Date(df$Dates,"%m/%d/%y")

plot <- ggplot(data = df, aes(x=Dates, y=Variable_Data, 
                           colour=Variable_Name, group=1,
                           text = paste('Fecha: ', Dates,
                                        '<br>Linea:', Variable_Name, 
                                        '<br>Avance:', Variable_Data,'%'))) + 
        geom_line(size = 1) + labs(colour = "") + xlab("Fecha") + ylab("Avance") + theme_grey(base_size = 20)
####################################################

############# Create and save widget ###############
p = ggplotly(plot, tooltip = "text")%>%
  layout(hovermode = "x")
internalSaveWidget(p, 'out.html');
####################################################
