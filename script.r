source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly");
libraryRequireInstall("reshape2")
####################################################

################### Actual code ####################
############################################
orig_dates = as.Date(as.vector(as.matrix(dates)))
orig_plan = as.vector(as.matrix(plan))
orig_real = as.vector(as.matrix(real))
orig_change_control = as.vector(as.matrix(change_control))
if(exists("request")){
  orig_request = as.vector(as.matrix(request))
} else {
  orig_request = 0
}
real_positions = which(orig_real != "")
change_control =which(orig_change_control != "")

if(range(orig_plan)[2]<=1){
  orig_plan = orig_plan*100
}
if(range(orig_real[real_positions])[2]<=1){
  orig_real = orig_real*100
}

if(length(unique(orig_request)) <= 1){
  if(exists("change_control"))
  {
    if(length(change_control)){
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
    } else {
      dates = orig_dates
      progress = orig_plan
      base_line = c()
      for(i in 1:length(dates)){
        base_line = c(base_line, "PLAN")
      }
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
  ############# Create and save widget ###############
  ############
  p = ggplotly(plot, tooltip = "text")%>%
  layout(hovermode = "x")
  ############
  internalSaveWidget(p, 'out.html');
  ####################################################
} else {
  ua=unique(orig_request)
  bp=c()
  cp=c()
  for(i in 1:length(ua)){
    print(i)
    bfn=orig_plan[which(orig_request==ua[i])]
    cfn=orig_real[which(orig_request==ua[i])]
    bp=c(bp,bfn[max(which(cfn != ""))])
    cp=c(cp,cfn[max(which(cfn != ""))])
  }
  data=data.frame("FN"=ua,"PLAN"=bp,"REAL"=cp)
  data <- melt(data, id="FN")
  names(data)=c("FN","VARIABLE","AVANCE")

  plot=ggplot(data,aes(x=FN,y=AVANCE,fill=VARIABLE))+
    geom_bar(stat="identity",position="dodge")+
    xlab("SOLICITUD")+ylab("AVANCE")+ theme_grey(base_size = 14)

  ggplotly(plot)
  ############# Create and save widget ###############
  ############
  p = ggplotly(plot)
  ############
  internalSaveWidget(p, 'out.html');
  ####################################################
}
############################################
####################################################

############# Create and save widget ###############
############

############
####################################################
