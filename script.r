source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
####################################################

################### Actual code ####################
dataset = data.frame(Value)

dataset$FECHA = as.Date(dataset$SEMANA)
somos =which(dataset$SOMO != "")
somos=c(somos,length(dataset$SEMANA))
FECHA_PLAN=dataset$FECHA[1:somos[1]]
PLAN=dataset$PLAN[1:somos[1]]
plot = ggplot()+ geom_line(aes(x=FECHA_PLAN, y=PLAN, color="PLAN"))

l = length(dataset)
lineas_somos = array()
i=0
FECHA = list()
PLAN_SOMO = list()
colour=list()
data_label = list()
while (i < length(somos)-1) {
  i=i+1
  print(i)
  FECHA[[i]]=(dataset$FECHA[(somos[i]+1):somos[i+1]])
  PLAN_SOMO[[i]]=dataset$PLAN[(somos[i]+1):somos[i+1]]
  colour[[i]]=(paste('PLAN SOMO',i))
  data_label[[i]] = paste('Plan:', dataset$PLAN[(somos[i]+1):somos[i+1]],
                          '<br>Date: ', (dataset$FECHA[(somos[i]+1):somos[i+1]]),
                          '<br>Somo: ', i)
}
i=0
while (i < length(somos)-1) {
  i=i+1
  loop_input = paste("geom_line(aes(x=FECHA[[",i,"]], y=PLAN_SOMO[[",i,"]], color=colour[[",i,"]], text='",data_label[[i]],"'))")
  plot = plot + eval(parse(text=loop_input))
  lineas_somos[somos[i]] = 0.5
}
FECHA_REAL=dataset$FECHA
REAL=dataset$REAL
SOMO=dataset$FECHA[somos]
plot = plot + geom_bar(aes(x=SOMO), width = 0.25, colour="darkgoldenrod")
plot = plot + geom_line(aes(x=FECHA_REAL, y=REAL, color="REAL"))
plot = plot + labs(x = "Fecha", 
       y = "Avance", 
       colour = "")
####################################################

############# Create and save widget ###############
p = ggplotly(plot, tooltip = c("text"), margin=dict(t=50));
internalSaveWidget(p, 'out.html');
####################################################
