#This script will update the plots available @ shorestations.ucsd.edu for SC, NB, Zuma, SB, GC, PG.
##It pulls data directly from the server, plots it, and puts the plot onto google drive.
##The webiste then pulls the plots directly from google drive with a linked url.

#start of every script
rm(list=ls())
#SC###################################################################################################################
#set the working directory to match the server location of the latest Temperature data from SC
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/2_SC")
#load in some libraries that will become essential later
library(stringr)
library(readxl)
library(googledrive)
library(stringr)
#read in the latest excel file in the working directory as 'stack' 
##The line below calls for all the files in the working directory with "dir()".
###It then extracts the date string from all the file names in the directory with the series of 8 '\\d' below.  So in theory, it is looking for 8 consecutuve numbers in each file name and extracting them.
####It then figures out which of the 8 digit number strings is highest, figures out what the file name is, and loads it in as 'stack'
stack=as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1))
#Since all of the manual shore station files have a header, let's remove it.
##We scan through the first row of data to isolate the row in which the word 'YEAR' is stated.  This should be the start of the file.
which(stack[,1]=='YEAR')
#So we can remove all the rows above that point.
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$SURF_TEMP_C=suppressWarnings(as.numeric(stack$SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$TEMP_FLAG[is.nan(stack$TEMP_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$TEMP_FLAG=='0' | today$TEMP_FLAG=='7',]
#This year so far...
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
#df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year SC.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("San Clemente SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='purple')
#legend('topleft',legend=as.vector(c('Record High in Previous Years','All Time Average','Record Low',paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste('Record High Set in',substr(Sys.Date(),1,4)),paste('Record Low Set in',substr(Sys.Date(),1,4)))),col=c('maroon','black','blue','orange','red','blue'),lty=c(1,2,1,1,NA,NA),lwd=c(1,1,1,2,NA,NA),pch=c(NA,NA,NA,NA,20,20),bty='n')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1955-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
#and put them all on the team drive
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/185Or6LkcEZWQUkYN7jGvPUtEXWBV3NrB/view?usp=sharing"),media=paste(getwd(),"/SST This Year SC.pdf",sep=''))



#NB###################################################################################################################
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/3_NEWPORT")
stack=as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1))
which(stack[,1]=='YEAR')
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$SURF_TEMP_C=suppressWarnings(as.numeric(stack$SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$TEMP_FLAG[is.nan(stack$TEMP_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$TEMP_FLAG=='0' | today$TEMP_FLAG=='7',]
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year NB.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Newport Beach SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1925-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/15lKxyRuPa6GA1PkYuC9F2g25p_ie7GPU/view?usp=sharing"),media=paste(getwd(),"/SST This Year NB.pdf",sep=''))

#Zuma###################################################################################################################
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/4_ZUMA")
stack=suppressWarnings(as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1)))
which(stack[,1]=='YEAR')
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$SURF_TEMP_C=suppressWarnings(as.numeric(stack$SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$TEMP_FLAG[is.nan(stack$TEMP_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$TEMP_FLAG=='0' | today$TEMP_FLAG=='7',]
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
stack2=stack2[-c(which(is.na(stack2$MonthDayChar))),]
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST[1]}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year Zuma.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Zuma Beach SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1956-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1S4Fvd_fYpWHzr_dKOWhMAxNHmACJzbtV/view?usp=sharing"),media=paste(getwd(),"/SST This Year Zuma.pdf",sep=''))


#SB###################################################################################################################
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/5_SB")
stack=suppressWarnings(as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1)))
which(stack[,1]=='YEAR')
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$SURF_TEMP_C=suppressWarnings(as.numeric(stack$SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$SURF_FLAG[is.nan(stack$SURF_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$SURF_FLAG=='0' | today$SURF_FLAG=='7',]
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year SB.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Santa Barbara SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1946-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1eYM0JawrgPJR5X-4NfhCImMFROOXXhCp/view?usp=sharing"),media=paste(getwd(),"/SST This Year SB.pdf",sep=''))


#GC###################################################################################################################
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/6_GC")
stack=suppressWarnings(as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1)))
which(stack[,1]=='YEAR')
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$SURF_TEMP_C=suppressWarnings(as.numeric(stack$SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$TEMP_FLAG[is.nan(stack$TEMP_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$TEMP_FLAG=='0' | today$TEMP_FLAG=='7',]
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year GC.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Granite Canyon SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1971-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
#and put them all on the team drive
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/18YEp0WzviJHKscLjH7xNJu-y8qu43GeK/view?usp=sharing"),media=paste(getwd(),"/SST This Year GC.pdf",sep=''))


#PG###################################################################################################################
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/7_PG")
stack=suppressWarnings(as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1)))
which(stack[,1]=='YEAR')
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$SURF_TEMP_C=suppressWarnings(as.numeric(stack$SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$TEMP_FLAG[is.nan(stack$TEMP_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$TEMP_FLAG=='0' | today$TEMP_FLAG=='7',]
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year PG.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Pacific Grove SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1919-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
#and put them all on the team drive
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/12HrvEJztPNhXGGwc_74m5pMcZqg_2KeU/view?usp=sharing"),media=paste(getwd(),"/SST This Year PG.pdf",sep=''))




#SIO###############
rm(list=ls())
library(googledrive)
library(stringr)
drive_download(drive_get(id="1Rfl69IuXa-UIgjdZX3MozJIhOCcVnRaATLLdwq7E52w"),type='csv',overwrite=T)
stack=read.csv("non-QC'd SIO Shore Station Data.csv",header=T,sep=',')
stack$time=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),substr(as.character(stack$TIME_PST),1,2),substr(as.character(stack$TIME_PST),3,4),sep='-'),'%Y-%m-%d-%H-%M'))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$SURF_FLAG=='0' | today$SURF_FLAG=='7',]

pdf("History of SST Today.pdf",width=11,height=8.5,encoding='MacRoman')
plot(y=c(min(na.omit(today$SURF_TEMP_C)-2),max(na.omit(today$SURF_TEMP_C))+2),x=rep(1,2),col='blue',main=paste("The History of Scripps Pier SST (C) for ",substr(Sys.Date(),1,4),"-",tail(stack$MonthDayChar,1),sep=''),ylab="Temperature (C)",xlab="",xaxt="n",type='n')
boxplot(today$SURF_TEMP_C~rep(1,length(today$SURF_TEMP_C)),add=T,cex=1.2)
today=today[order(-today$SURF_TEMP_C,-today$YEAR),]
points(tail(stack$SURF_TEMP_C,1),col='orange',pch=20,cex=2)
points(tail(today$SURF_TEMP_C,1),col='blue',pch=20,cex=2)
points(head(today$SURF_TEMP_C,1),col='maroon',pch=20,cex=2)
library(toOrdinal)
legend('topleft',legend=as.vector(c(paste("Today's SST is the",toOrdinal(which(as.character(today$YEAR)==substr(Sys.Date(),1,4))),"warmest",month.abb[as.numeric(substr(tail(stack$MonthDayChar,1),1,2))],toOrdinal(as.numeric(substr(tail(stack$MonthDayChar,1),4,5))),"on record @",today$SURF_TEMP_C[today$YEAR==substr(Sys.Date(),1,4)]),paste(today$YEAR[1],'-',today$MonthDayChar[1],' was the warmest @ ',today$SURF_TEMP_C[1],sep=''),paste("The average temperature for",month.abb[as.numeric(substr(today$MonthDayChar,1,2))[1]],toOrdinal(as.numeric(substr(today$MonthDayChar[1],4,5))),"is",round(mean(today$SURF_TEMP_C[today$YEAR!=substr(Sys.Date(),1,4)]),1)),paste(today$YEAR[length(today$YEAR)],"-",today$MonthDayChar[1],' was the coldest @ ',today$SURF_TEMP_C[length(today$YEAR)],sep=''))),bty='n',pch=c(20,20,NA,20),col=c('orange','maroon','black','blue'),lwd=c(3,3,3,3),lty=c(NA,NA,1,NA))
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()



bottom=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
bottom=bottom[is.nan(bottom$BOT_TEMP_C)==F,]
bottom=bottom[bottom$BOT_FLAG=='0' | bottom$BOT_FLAG=='7',]

if(as.character(tail(stack$BOT_TEMP_C,1))!="NaN"){
  pdf("History of SBT Today.pdf",width=11,height=8.5,encoding='MacRoman')
  plot(y=c(min(na.omit(bottom$BOT_TEMP_C)-2),max(na.omit(bottom$BOT_TEMP_C))+2),x=rep(1,2),col='blue',main=paste("The History of Scripps Pier SBT (C) for ",substr(Sys.Date(),1,4),"-",tail(stack$MonthDayChar,1),sep=''),ylab="Temperature (C)",xlab="",xaxt="n",type='n')
  boxplot(bottom$BOT_TEMP_C~rep(1,length(bottom$BOT_TEMP_C)),add=T,cex=1.2)
  bottom=bottom[order(-bottom$BOT_TEMP_C,-bottom$YEAR),]
  points(tail(stack$BOT_TEMP_C,1),col='orange',pch=20,cex=2)
  points(tail(bottom$BOT_TEMP_C,1),col='blue',pch=20,cex=2)
  points(head(bottom$BOT_TEMP_C,1),col='maroon',pch=20,cex=2)
  legend('topleft',legend=as.vector(c(paste("Today's SBT is the",toOrdinal(which(as.character(bottom$YEAR)==substr(Sys.Date(),1,4))),"warmest",month.abb[as.numeric(substr(tail(stack$MonthDayChar,1),1,2))],toOrdinal(as.numeric(substr(tail(stack$MonthDayChar,1),4,5))),"on record @",bottom$BOT_TEMP_C[bottom$YEAR==substr(Sys.Date(),1,4)]),paste(bottom$YEAR[1],"-",bottom$MonthDayChar[1],' was the warmest @ ',bottom$BOT_TEMP_C[1],sep=''),paste("The average temperature for",month.abb[as.numeric(substr(bottom$MonthDayChar,1,2))[1]],toOrdinal(as.numeric(substr(today$MonthDayChar[1],4,5))),"is",round(mean(bottom$BOT_TEMP_C[bottom$YEAR!=substr(Sys.Date(),1,4)]),1)),paste(bottom$YEAR[length(bottom$YEAR)],"-",bottom$MonthDayChar[1],' was the coldest @ ',bottom$BOT_TEMP_C[length(bottom$YEAR)],sep=''))),bty='n',pch=c(20,20,NA,20),col=c('orange','maroon','black','blue'),lwd=c(3,3,3,3),lty=c(NA,NA,1,NA))
  legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
  legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
  dev.off()
}


stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA,BOTmin=NA,BOTmax=NA,BOTmean=NA,BOTthisYear=NA,SURFlastYear=NA,BOTlastYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA,min(as.numeric(na.omit(stack2$BOT_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$BOT_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$BOT_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA,NA,NA))
}
df=df[-1,]
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)

for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
  df$BOTthisYear[i]=stack$BOT_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}


for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  SBT=stack$BOT_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
  if(length(SBT)!=0){df$BOTlastYear[i]=SBT}
}

df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$BOTlastYear[which(df$BOTlastYear=="NaN")]=NA


pdf("SST This Year.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Scripps Pier SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFmean~df$time,type='l',col='black',lty=2)
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[df$SURFthisYear>df$SURFmax]))~na.omit(df$time[df$SURFthisYear>df$SURFmax]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[df$SURFthisYear<df$SURFmin]))~na.omit(df$time[df$SURFthisYear<df$SURFmin]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1916-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1, 'SST'),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA),pch=c(15,NA,NA,NA,20),col=c('gray','black','gray50','blue','red'))
legend('topright',legend=as.vector(c(paste(month.abb[as.numeric(substr(today$MonthDayChar[1],1,2))],toOrdinal(as.numeric(substr(today$MonthDayChar,4,5))[1]),substr(Sys.Date(),1,4),sep=' '),paste("Today's SST=",(today$SURF_TEMP_C[today$YEAR==substr(Sys.Date(),1,4)]*9/5)+32,"F/",today$SURF_TEMP_C[today$YEAR==substr(Sys.Date(),1,4)],'C'))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()


df$BOTthisYear=as.numeric(df$BOTthisYear)
df$BOTmax=as.numeric(df$BOTmax)
pdf("SBT This Year.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Scripps Pier SBT (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$BOTmax,rev(df$BOTmin)),col='gray',border=NA)
lines(df$BOTlastYear[is.na(df$BOTlastYear)==F]~df$time[is.na(df$BOTlastYear)==F],type='l',col='gray50')
lines(df$BOTmean~df$time,type='l',col='black',lty=2)
lines(df$BOTthisYear[is.na(df$BOTthisYear)==F]~df$time[is.na(df$BOTthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$BOTthisYear[df$BOTthisYear>df$BOTmax]))~na.omit(df$time[df$BOTthisYear>df$BOTmax]),pch=20,col='red')
points(as.numeric(na.omit(df$BOTthisYear[df$BOTthisYear<df$BOTmin]))~na.omit(df$time[df$BOTthisYear<df$BOTmin]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1916-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,NA),pch=c(15,NA,NA,20),col=c('gray','black','blue','red'))
legend('topright',legend=as.vector(c(paste(month.abb[as.numeric(substr(today$MonthDayChar[1],1,2))],toOrdinal(as.numeric(substr(today$MonthDayChar,4,5))[1]),substr(Sys.Date(),1,4),sep=' '),paste("Today's SBT=",(bottom$BOT_TEMP_C[bottom$YEAR==substr(Sys.Date(),1,4)]*9/5)+32,"F/",bottom$BOT_TEMP_C[bottom$YEAR==substr(Sys.Date(),1,4)],'C'))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()

ThisMonth=stack[stack$MONTH==today$MONTH[1],]
ThisMonthSURF=ThisMonth[-c(which(is.na(ThisMonth$SURF_TEMP_C))),]


pdf("MonthlyReviewSST_SIO.pdf",width=11,height=8.5)
boxplot(ThisMonthSURF$SURF_TEMP_C,ylab='Temperature (C)',main=paste(month.name[today$MONTH[1]],'SST at SIO'),ylim=c(range(ThisMonthSURF$SURF_TEMP_C)[1]-3,range(ThisMonthSURF$SURF_TEMP_C)[2]))
lapply(ThisMonthSURF$SURF_TEMP_C[which(ThisMonthSURF$YEAR==substr(Sys.Date(),1,4))],function(x){points(x,col='forestgreen',pch=20,cex=2)})
points(ThisMonthSURF$SURF_TEMP_C[which(ThisMonthSURF$date==today$date[today$YEAR==substr(Sys.Date(),1,4)])],col='orange',pch=20,cex=2)
points(max(ThisMonthSURF$SURF_TEMP_C),col='maroon',pch=20,cex=2)
legend("bottomleft",legend=as.vector(c(paste("The average of all observations in ",month.abb[ThisMonthSURF$MONTH[1]],' (',min(ThisMonthSURF$YEAR),'-',max(ThisMonthSURF$YEAR),') is ',round(mean(ThisMonthSURF$SURF_TEMP_C),1),' C/ ',round((round(mean(ThisMonthSURF$SURF_TEMP_C),1)*9/5)+32,1),' F',sep=''),paste("The observations from ",month.abb[ThisMonthSURF$MONTH[1]],' ',max(ThisMonthSURF$YEAR),' (up to ',month.abb[ThisMonthSURF$MONTH[1]],' ',toOrdinal(today$DAY[1]-1),')',sep=''),paste("The Temperature from",month.abb[ThisMonthSURF$MONTH[1]],toOrdinal(today$DAY[1]),max(ThisMonthSURF$YEAR),'was',today$SURF_TEMP_C[which(today$YEAR==max(today$YEAR))],'C /',round((today$SURF_TEMP_C[which(today$YEAR==max(today$YEAR))]*9/5)+32,1),'F'),paste("The warmest temperature ever recorded in",month.abb[ThisMonthSURF$MONTH[1]],'was',max(ThisMonthSURF$SURF_TEMP_C),'C /',round((max(ThisMonthSURF$SURF_TEMP_C)*9/5)+32,1),'F','on',month.abb[ThisMonthSURF$MONTH[1]],toOrdinal(ThisMonthSURF$DAY[tail(which(ThisMonthSURF$SURF_TEMP_C==max(ThisMonthSURF$SURF_TEMP_C)),1)]),ThisMonthSURF$YEAR[tail(which(ThisMonthSURF$SURF_TEMP_C==max(ThisMonthSURF$SURF_TEMP_C)),1)]))),lty=c(1,NA,NA,NA),pch=c(NA,20,20,20),lwd=c(2,2,2,2),col=c("black","forestgreen","orange",'maroon'),bty='n')
legend('bottomright',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()

#and put them all on the team drive
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/18nvAiMpP9HI6yzxr8CvUjlfWZvqjZGMt/view?usp=sharing"),media=paste(getwd(),"/History of SBT Today.pdf",sep=''))
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1gCV4-bzI8grV8VJrElG2T2ZXt_39MVPC/view?usp=sharing"),media=paste(getwd(),"/History of SST Today.pdf",sep=''))
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1sLb41z6XzhsJuJnNAvAyjmYl5M6pw8gK/view?usp=sharing"),media=paste(getwd(),"/SBT This Year.pdf",sep=''))
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1jK4xHp1fo6x_-r8CdGPCDoz3zG82IPdI/view?usp=sharing"),media=paste(getwd(),"/SST This Year.pdf",sep=''))
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1wh39yiA-blbYWmI4zRz4zTUM80KrWlPS/view?usp=sharing"),media=paste(getwd(),"/MonthlyReviewSST_SIO.pdf",sep=''))




#email about no sample taken
library(mailR)
sender="jfumo@ucsd.edu"
recipients=c('shorestation@ucsd.edu')
BodyNote="Nobody sampled today for the SIO MSS today...  A pier walk would be nice wouldn't it?"
subject=paste("Have time for a pier walk?",Sys.Date()-1,sep=' ')
CurrentTime_Local=format(Sys.time(), tz="America/Los_Angeles",usetz=TRUE)
DateMatch=as.Date(tail(stack$date,1))!=as.Date(substr(CurrentTime_Local,1,10))
TimeMatch=as.numeric(substr(CurrentTime_Local,12,13))==17
if(DateMatch==T & TimeMatch==T){send.mail(from=sender,to=recipients,subject=subject,body=BodyNote,smtp=list(host.name="smtp.ucsd.edu",port=25,user.name=sender),send=T)}

#email about record temperatures
subject=paste("PRELIMINARY DATA! New record for",month.abb[today$MONTH[1]],toOrdinal(today$DAY[1]))
recipients=c('shorestation@ucsd.edu')
BodyNote=paste("Today's SST is the",toOrdinal(which(today$YEAR==max(today$YEAR))),'warmest',month.abb[today$MONTH[1]],toOrdinal(today$DAY[1]),'on record at',today$SURF_TEMP_C[which(today$YEAR==max(today$YEAR))],'C /',round((today$SURF_TEMP_C[which(today$YEAR==max(today$YEAR))]*9/5)+32,1),'F')
emailTracker=suppressWarnings(read.csv("emailTracker.csv",sep=',',stringsAsFactors = F,header=T))
DateMatch=emailTracker$EmailSentOn[1]!=as.character(today$date[today$YEAR==max(today$YEAR)])
TopFive=which(today$YEAR==max(today$YEAR))<=5
if(TopFive==T & DateMatch==T){send.mail(from=sender,to=recipients,subject=subject,body=BodyNote,smtp=list(host.name="smtp.ucsd.edu",port=25,user.name=sender),attach.files=c(paste(getwd(),'/MonthlyReviewSST_SIO.pdf',sep=''),paste(getwd(),'/History of SST Today.pdf',sep=''),paste(getwd(),'/History of SBT Today.pdf',sep='')),send=T);emailTracker$EmailSentOn[1]=as.character(today$date[today$YEAR==max(today$YEAR)]);write.csv(emailTracker,'emailTracker.csv',row.names = F)}

#FAR##############
rm(list=ls())
library(googledrive)
library(stringr)
drive_download(drive_get(id="1c_p1O8FDb-vw038_4GeaFB24PimSXXAJASeHRGo8ZgY"),type='csv',overwrite=T)
stack=read.csv("non-QC'd Farallon Shore Staion Data.csv",header=T,sep=',')
stack$time=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),substr(as.character(stack$TIME_PST),1,2),substr(as.character(stack$TIME_PST),3,4),sep='-'),'%Y-%m-%d-%H-%M'))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$FLAG_TEMP[is.nan(stack$FLAG_TEMP)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$FLAG_TEMP=='0' | today$FLAG_TEMP=='7',]

pdf("History of SST Today FAR.pdf",width=11,height=8.5,encoding='MacRoman')
plot(y=c(min(na.omit(today$SURF_TEMP_C)-2),max(na.omit(today$SURF_TEMP_C))+2),x=rep(1,2),col='blue',main=paste("The History of SE Farallon Island SST (C) on 2018-",tail(stack$MonthDayChar,1),sep=''),ylab="Temperature (C)",xlab="",xaxt="n",type='n')
boxplot(today$SURF_TEMP_C~rep(1,length(today$SURF_TEMP_C)),add=T,cex=1.2)
today=today[order(-today$SURF_TEMP_C,-today$YEAR),]
points(tail(stack$SURF_TEMP_C,1),col='orange',pch=20,cex=2)
points(tail(today$SURF_TEMP_C,1),col='blue',pch=20,cex=2)
points(head(today$SURF_TEMP_C,1),col='maroon',pch=20,cex=2)
library(toOrdinal)
legend('topleft',legend=as.vector(c(paste("Today's SST is the",toOrdinal(which(as.character(today$YEAR)==substr(Sys.Date(),1,4))),"warmest",month.abb[as.numeric(substr(tail(stack$MonthDayChar,1),1,2))],toOrdinal(as.numeric(substr(tail(stack$MonthDayChar,1),4,5))),"on record @",today$SURF_TEMP_C[today$YEAR==substr(Sys.Date(),1,4)]),paste(today$YEAR[1],'was the warmest @',today$SURF_TEMP_C[1]),paste(today$YEAR[length(today$YEAR)],'was the coldest @',today$SURF_TEMP_C[length(today$YEAR)]))),bty='n',pch=c(20,20,20),col=c('orange','maroon','blue'),lwd=c(3,3,3),lty=c(NA,NA,NA))
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()

stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)

for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]][1]
}

for(i in 1:length(df$day)){
  SST=stack$SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]][1]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}

df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA

pdf("SST This Year FAR.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("SE Farallon Island SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(df$SURFmean~df$time,type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1925-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,NA),pch=c(15,NA,NA,20),col=c('gray','black','blue','red'))
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()

drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1UXf1k5MZjCtODL2U5tqmAKccpAyVK9pF/view?usp=sharing"),media=paste(getwd(),"/History of SST Today FAR.pdf",sep=''))
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1F5SNW3OvSR84OgsdgiuO5k_St4KM6CKi/view?usp=sharing"),media=paste(getwd(),"/SST This Year FAR.pdf",sep=''))



#HSU Beach###################################################################################################################
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/9_HSU")
stack=suppressWarnings(as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1)))
which(stack[,1]=='YEAR')
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$TRINIDAD_BEACH_SURF_TEMP_C=suppressWarnings(as.numeric(stack$TRINIDAD_BEACH_SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$BEACH_FLAG[is.nan(stack$BEACH_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$BEACH_FLAG=='0' | today$BEACH_FLAG=='7',]
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$TRINIDAD_BEACH_SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$TRINIDAD_BEACH_SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$TRINIDAD_BEACH_SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$TRINIDAD_BEACH_SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$TRINIDAD_BEACH_SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)
df=df[-which(is.nan(as.numeric(df$SURFmean))),]

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year HSU Beach.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Trinidad Beach SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1973-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$TRINIDAD_BEACH_SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
#and put them all on the team drive
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1xsmK5puvPz_UKQ0eQUYIohb9edhPh3iX/view?usp=sharing"),media=paste(getwd(),"/SST This Year HSU Beach.pdf",sep=''))



#HSU Bay###################################################################################################################
setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/CURRENT/9_HSU")
stack=suppressWarnings(as.data.frame(read_excel(as.character(na.omit(dir()[str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')==max(na.omit(str_extract(dir(),'\\d\\d\\d\\d\\d\\d\\d\\d')))])),1)))
which(stack[,1]=='YEAR')
stack=stack[c(which(stack[,1]=='YEAR'):length(stack[,1])),]
colnames(stack)=stack[1,]
stack=stack[-1,]
stack$TRINIDAD_BAY_SURF_TEMP_C=suppressWarnings(as.numeric(stack$TRINIDAD_BAY_SURF_TEMP_C))
stack$date=as.POSIXct(strptime(paste(as.character(stack$YEAR),as.character(stack$MONTH),as.character(stack$DAY),sep='-'),'%Y-%m-%d'))
stack$MonthDayChar=paste(str_pad(as.character(stack$MONTH),side='left',pad='0',width=2),str_pad(as.character(stack$DAY),side='left',pad='0',width=2),sep='-')
stack$BAY_FLAG[is.nan(stack$BAY_FLAG)==T]=0
today=stack[stack$MonthDayChar==tail(stack$MonthDayChar,1),]
today=today[is.nan(today$SURF_TEMP_C)==F,]
today=today[today$BAY_FLAG=='0' | today$BAY_FLAG=='7',]
stack2=stack[stack$YEAR!=as.numeric(substr(Sys.Date(),1,4)),]
df=data.frame(day=NA,SURFmin=NA,SURFmax=NA,SURFmean=NA,SURFthisYear=NA)
for(i in 1:length(unique(stack2$MonthDayChar))){
  df=rbind(df,c(unique(stack2$MonthDayChar)[i],min(as.numeric(na.omit(stack2$TRINIDAD_BAY_SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),max(as.numeric(na.omit(stack2$TRINIDAD_BAY_SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),mean(as.numeric(na.omit(stack2$TRINIDAD_BAY_SURF_TEMP_C[stack2$MonthDayChar==unique(stack2$MonthDayChar)[i]]))),NA))
}
df=df[-1,]
df$dayYear=paste("2000",df$day,sep='-')
df$time=as.POSIXct(strptime(df$dayYear,'%Y-%m-%d'))
df=df[order(df$time),]
rm(stack2)
for(i in 1:length(df$day)){
  if(length(stack$YEAR[stack$YEAR==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]])==0){next}
  df$SURFthisYear[i]=stack$TRINIDAD_BAY_SURF_TEMP_C[as.character(stack$YEAR)==substr(Sys.Date(),1,4) & stack$MonthDayChar==unique(df$day)[i]]
}
for(i in 1:length(df$day)){
  SST=stack$TRINIDAD_BAY_SURF_TEMP_C[stack$YEAR==as.numeric(substr(Sys.Date(),1,4))-1 & stack$MonthDayChar==df$day[i]]
  if(length(SST)!=0){df$SURFlastYear[i]=SST}
}
df$SURFlastYear[which(df$SURFlastYear=="NaN")]=NA
df$SURFthisYear=as.numeric(df$SURFthisYear)
df=df[-which(is.nan(as.numeric(df$SURFmean))),]

setwd("/home/rstudio/jfumo/sccoos/manual_shore_station/DATA/TEMPERATURE/AutomatedPlotsForMSSwebsite")
pdf("SST This Year HSU Bay.pdf",width=11,height=8.5,encoding='MacRoman')
plot(x=range(df$time),y=range(c(as.numeric(na.omit(as.numeric(df$SURFmin))),as.numeric(na.omit(as.numeric(df$SURFmax))),as.numeric(na.omit(as.numeric(df$SURFmean))),as.numeric(na.omit(as.numeric(df$SURFthisYear))),as.numeric(na.omit(as.numeric(df$BOTmin))),as.numeric(na.omit(as.numeric(df$BOTmax))),as.numeric(na.omit(as.numeric(df$BOTthisYear))))),type='n',ylab="Temperature (C)",xlab="Date",main=paste("Trinidad Bay SST (C) In",substr(Sys.Date(),1,4)))
polygon(c(df$time,rev(df$time)),c(df$SURFmax,rev(df$SURFmin)),col='gray',border=NA)
lines(SURFmean~time,data=df[is.nan(df$SURFmean)==F,],type='l',col='black',lty=2)
lines(df$SURFlastYear[is.na(df$SURFlastYear)==F]~df$time[is.na(df$SURFlastYear)==F],type='l',col='gray50')
lines(df$SURFthisYear[is.na(df$SURFthisYear)==F]~df$time[is.na(df$SURFthisYear)==F],type='l',col='blue',lwd=2)
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]))~na.omit(df$time[as.numeric(df$SURFthisYear)>as.numeric(df$SURFmax)]),pch=20,col='red')
points(as.numeric(na.omit(df$SURFthisYear[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]))~na.omit(df$time[as.numeric(df$SURFthisYear)<as.numeric(df$SURFmin)]),pch=20,col='blue')
legend('topleft',legend=as.vector(c(paste("Julian Day Range of Observed Temperatures (1973-",as.character(as.numeric(substr(Sys.Date(),1,4))-1),')',sep=''),'Average Annual Cycle',paste(as.numeric(substr(Sys.Date(),1,4))-1,"SST"),paste(substr(Sys.Date(),1,4),"(Preliminary)"),paste("Julian Day Record High (",substr(Sys.Date(),1,4),')',sep=''),paste("Julian Day Record Low (",substr(Sys.Date(),1,4),')',sep=''))),bty='n',lty=c(NA,2,1,1,NA,NA),pch=c(15,NA,NA,NA,20,20),col=c('gray','black','gray50','blue','red','purple'))
legend('topright',legend=as.vector(c(paste('Latest data from:',substr(stack$date[max(which(is.na(stack$TRINIDAD_BAY_SURF_TEMP_C)==F))],1,10)))),bty='n')
legend('bottomright',legend=as.vector(c("Manual Shore Station Program","California State Parks Division of Boating and Waterways","scripps.ucsd.edu/programs/shorestations/")),text.col=c('black','black','blue'),bty='n',cex=.8)
legend('bottomleft',legend=as.vector(c("Fumo/ Carter")),col='gray',bty='n',cex=.75)
dev.off()
#and put them all on the team drive
drive_update(file=as_id("https://drive.google.com/a/ucsd.edu/file/d/1ZXZ7XT9dZv4cVZtBNAU2xSQ70K15Fsue/view?usp=sharing"),media=paste(getwd(),"/SST This Year HSU Bay.pdf",sep=''))
