library(signal)
library(zoo)
library(plyr)
library(R.matlab)
library(multcomp)
library(e1071)
library(snowfall)

setwd("/home/rcf-proj/jrm/amelrose/Psych625/test_1/")

#570
n=1478#number of files to read and to load later
varlist <- paste("1_0_",1:n,sep="")#the name of the variables to create
#varlist <- paste("new_1_",1:n,sep="")
matlist <- paste("readMat('1_0_",1:n,".mat')", sep="")#These are the names of the files to load
#matlist <- paste("readMat('new_1_",1:n,".mat')", sep="")
# change to "_1.mat" if you want to read pre-ictal files
eq <- paste(paste(varlist, matlist, sep="<-"), collapse=";")
eval(parse(text=eq))

#to load multiple files
varlist2 <- paste("1_0_",1:n,sep="")#that will be the name of the datasets
#varlist2 <- paste("new_1_",1:n,sep="")
matlist2 <- paste("as.data.frame("1_0_",1:n,"[[1]][[1]])", sep="")#to subset the datasets for each file
#matlist2 <- paste("as.data.frame(new_1_",1:n,"[[1]][[1]])", sep="")

eq2 <- paste(paste(varlist2, matlist2, sep="<-"), collapse=";")
eval(parse(text=eq2))

sampleRate = 400
low = 30
high = 2
Fs = sampleRate
Fn = Fs/2
Ws1 = c(low)/Fn
Ws2 = c(high)/Fn
ord = 3
bf1 <- butter(ord, Ws1, type="low")
bf2 <- butter(ord, Ws2, type="high")
time <- 600                   # measuring time interval (s)
# vector of sampling time-points (s)
smpl.int <- (1:(time*sampleRate))/sampleRate
preprocess <- function(x){tmp <- get(x)
	bpD <- data.frame(matrix(NA,ncol=ncol(tmp),nrow=1500))
	for(z in 1:ncol(tmp)){
  		tmp1 <- tmp[,z]
  		filtered1 <- filtfilt(bf1,c(rep(tmp1[1],200),tmp1,rep(tmp1[1],200)))
  		filtered1 <- filtered1[201:(length(filtered1)-200)]
  		filtered2 <- filtfilt(bf2,c(rep(filtered1[1],200),tmp1,rep(filtered1[1],200)))
  		filtered2 <- filtered2[201:(length(filtered2)-200)]
  		filtered2_fft <- fft(filtered2)
		phase.1 <- Arg(filtered2_fft)[1:(length(filtered2_fft)/2)]
		if((median(phase.1) != 0) | (mean(phase.1) != 0) | ((sum(is.na(phase.1))/nrow(tmp)) > 0.5)){
			phase_binned <- cut(phase.1,breaks=1500)
			phase_binned2 <- as.data.frame(table(phase_binned))
			output <- phase_binned2$Freq
			bpD[,z] <- output
		}
	}
	write.csv(bpD, file=paste0(x,"_processed.csv",sep=""))
	return(bpD)
}

#sfInit(parallel=TRUE,cpus=20,type="SOCK")

#sfLibrary(zoo)
#sfLibrary(signal)
#sfLibrary(stats)

#sfExportAll()

#blah <- sfLapply(varlist2, preprocess)
lapply(varlist2, preprocess)

#sfStop()

#write.csv(blah, file="bandpass_tester.csv")


#sampleRate = 400
#low = 30
#high = 2
#Fs = sampleRate
#Fn = Fs/2
#Ws1 = c(low)/Fn
#Ws2 = c(high)/Fn
#ord = 3
#bf1 <- butter(ord, Ws1, type="low")
#bf2 <- butter(ord, Ws2, type="high")
#for(z in 1:ncol(dat0_detrend)){
#  tmp1 <- dat0_detrend[,z]
#  x <- length(tmp1)
#  filtered1 <- filtfilt(bf1,c(rep(tmp1[1],200),tmp1,rep(tmp1[1],200)))
#  filtered1 <- filtered1[201:(length(filtered1)-200)]
#  x <- length(filtered1)
#  filtered2 <- filtfilt(bf1,c(rep(filtered1[1],200),tmp1,rep(filtered1[1],200)))
#  filtered2 <- filtered2[201:(length(filtered2)-200)]
#  dat0_detrend_bandpass[,z] <- filtered2
#}

#dat0_preprocessed_fft <- data.frame(matrix(NA,nrow=nrow(dat0),ncol=ncol(dat0)))
#for(b in 1:ncol(dat0)){
#  tempo <- dat0_detrend_bandpass[,b]
#  tempo_fft <- fft(tempo)
#  dat0_preprocessed_fft[,b] <- tempo_fft
#}

#acq.freq <- 400       # data acquisition frequency (Hz)
##time <- 17.5 #since this is the reduced dataset with 7000 lines
#time <- 600                   # measuring time interval (s)

# vector of sampling time-points (s)
#smpl.int <- (1:(time*acq.freq))/acq.freq  

#binned_phases <- data.frame(matrix(NA,nrow=0,ncol=1201))
for(s in 1:ncol(dat0_preprocessed_fft)){
	test <- dat0_preprocessed_fft[,s]

	# extract magnitudes and phases
	magn <- Mod(test) # sqrt(Re(test)*Re(test)+Im(test)*Im(test))
	phase <- Arg(test) # atan(Im(test)/Re(test))

	# select only first half of vectors
	magn.1 <- magn[1:(length(magn)/2)]
	phase.1 <- Arg(test)[1:(length(test)/2)]

	phase_binned <- cut(phase.1,breaks=1200)
	blah2 <- as.data.frame(table(phase_binned))
	blah3 <- c(s,blah2$Freq)
	binned_phases <- rbind(binned_phases,blah3)
}

set.seed(666)
tune.out<-tune(svm,Y~.,data=binned_phases,kernel="linear",ranges=list(cost=c(0.001,0.01,1,5,10,100)))

bestmod <- tune.out%best.model

testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)

