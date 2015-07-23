table(se$class)
names(se)
qda(class~shift+genergy+seismic+gpuls, data=se[250:370,])

cor(se$seismic,se$seismoacoustic)
levels(se[,1]) <- c(0,1)
levels(se[,2]) <- c(0,1,2)
se[,c(1,2)] <- apply(se[,c(1,2)],2,as.integer)



qda(class~seismic+seismoacoustic+gpuls+genergy+shift, data=se)
