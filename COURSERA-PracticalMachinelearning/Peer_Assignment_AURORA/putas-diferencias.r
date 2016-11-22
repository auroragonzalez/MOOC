
jj<-all.equal(testN, trainN)
str(jj)
jj

identical(testN, trainN)
identical(df2, df3)
which(testN != trainN)



str(testN)
class(testN$X)

k1<-sapply(trainN, class)
k2<-sapply(testN, class)
kk<-data.frame(k1,k2, k0=NA, stringsAsFactors = FALSE)
# str(kk)
head(kk)
# kk$sonig <- rep(NULL,nrow(kk)) 
for (i in 1: nrow(kk)){
    if (kk$k1[i]==kk$k2[i]) { kk$k0[i] <- TRUE }    
    else{ kk$k0[i]<- FALSE}
}