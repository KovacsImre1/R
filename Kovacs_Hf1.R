#2.
krumpli <- pi 
#3.
print(krumpli)
#4.
b <- 342*krumpli
print(b)
 #5.
sorozat <- 44:60
print(sorozat)

#6.
sorozat %% 2 == 0

#7.
print(b+sorozat)

#8
name <- c("Gizi", "Bela", "Jenõ")
factor_name <- factor(name)
str(factor_name)
View(factor_name)

#9.
name <- c("Gizi", "Jenõ", "Bela")
factor_name <- factor(name)
str(factor_name)
factor_name
reoder(factor_name, )
factor_name[3]
View(factor_name)
#10
paros <- seq(10,100, 2)
print(paros)
str(paros)

#11
M <- matrix((1:60)*(-1),12,5, byrow=FALSE)
M

#12
dim <- c(3,4,2,5)
str(dim)
tomb <- array(c(dim[3]*dim[4]), dim)
tomb

#13
getwd()
setwd("D:/Lenovo2019/Szociológia Ma/2. Félév/Statisztikai Programozás/Hf")
library(foreign) #külön packet
Dataset <-  read.spss("ESS7.sav", 
                      rownames=FALSE, 
                      stringsAsFactors=TRUE, 
                      tolower=FALSE, 
                      to.data.frame = TRUE)
warning()
str(Dataset)
View(Dataset)
