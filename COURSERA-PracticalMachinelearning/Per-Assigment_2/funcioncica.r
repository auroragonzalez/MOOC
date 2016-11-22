
v1 <- c(rep("A", 5), rep("B",5), rep("C",5))
v2 <- c(rep("a", 5), rep("b",3), rep("c",5), rep("d",2))
v3 <-rnorm(15)
v4 <-runif(15)
v5 <- c(rep("z", 5), rep("x",3), rep("v",5), rep("w",2))
v6 <- c(rep("z", 3), rep("x",3), rep("v",5), rep("w",2), rep("kk",2))

df<- data.frame(v1, v2, v3, v4, v5,v6)
str(df)

df[,sapply(df,is.numeric)]
df[,sapply(df,is.factor)]

is.factor(df$v5)
nlevels(df$v5)

is.factor(df$v4)
nlevels(df$v4)



flevels <- function(v, nl=6){
    # nl<- nl-1 # menos de nl niveles
    if (nlevels(v)< nl) return (TRUE)
    else return(FALSE)
}


df[,sapply(df,flevels)]
