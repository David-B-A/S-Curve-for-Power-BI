source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
####################################################

################### Actual code ####################
############################################
orig_dates = as.Date(as.vector(as.matrix(dates)))
orig_plan = as.vector(as.matrix(plan))
orig_real = as.vector(as.matrix(real))
orig_change_control = as.vector(as.matrix(change_control))

real_positions = which(orig_real != "")

if(range(orig_plan)[2]<=1){
  orig_plan = orig_plan*100
}
if(range(orig_real[real_positions])[2]<=1){
  orig_real = orig_real*100
}
real_positions = which(orig_real != "")

if(range(orig_plan)[2]<=1){
  orig_plan = orig_plan*100
}
if(range(orig_real[real_positions])[2]<=1){
  orig_real = orig_real*100
}
change_control =which(orig_change_control != "")
change_control=c(change_control,length(orig_dates))

dates = orig_dates[1:change_control[1]]
progress = orig_plan[1:change_control[1]]
base_line = c()
for(i in 1:change_control[1]){
  base_line = c(base_line, "PLAN")
}

for(j in 1:(length(change_control)-1)){
  dates= c(dates, orig_dates[(change_control[j]+1):change_control[j+1]])
  progress = c(progress, orig_plan[(change_control[j]+1):change_control[j+1]])
  for(k in (change_control[j]+1):(change_control[j+1])){
    base_line = c(base_line, paste("SOMO",j))
  }
}

dates= c(dates,orig_dates[real_positions])
progress = c(progress, orig_real[real_positions])
for(z in real_positions){
  base_line = c(base_line, paste("REAL"))
}

df <- data.frame(dates = dates,
                 variable_data=progress, Variable_Name=base_line)

plot <- ggplot(data = df, aes(x=dates, y=variable_data, 
                           colour=Variable_Name, group=1,
                           text = paste('Fecha: ', dates,
                                        '<br>Linea:', Variable_Name, 
                                        '<br>Avance:', variable_data,'%'))) + 
        geom_line(size = 1.2) + labs(colour = "") + xlab("Fecha") + ylab("Avance") + theme_grey(base_size = 14)

############################################
####################################################

############# Create and save widget ###############
############
p = ggplotly(plot, tooltip = "text")%>%
  layout(hovermode = "x")
############
internalSaveWidget(p, 'out.html');
####################################################
