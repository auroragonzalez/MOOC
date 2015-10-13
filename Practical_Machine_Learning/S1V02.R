library("kernlab")

data(spam)

plot(density(spam$your[spam$type=="nonspam"]), col= "blue")
lines(density(spam$your[spam$type=="spam"]), col="red")
abline(v=0.5)
prediction <- ifelse(spam$your > 0.5, "SPAM", "NONSPAM")
table(prediction, spam$type)/length(spam$type)

## accuracy: 0.45 + 0.29 = 0.751 así que el 75 % de las veces estamos en lo correcto.


## Vídeo 4
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size=19),]
spamLabel <- (smallSpam$type =="spam")*1+1
plot(smallSpam$capitalAve, col=spamLabel)

