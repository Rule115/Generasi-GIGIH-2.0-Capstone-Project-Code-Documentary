#Package yang dibutuhkan
library(psych)
library(lmtest)
library(car)
library(nortest)

#Penentuan working directory
setwd("C:/Nu aing/Coolyeah/Generasi Gigih/Capstone Project")
setwd("C:/Users/SYAHRULGHANI/Downloads")

#Memanggil data
data = read.csv(file.choose(), head=T, sep=";")
data
attach(data)


#Melihat struktur data
str(data)
cor(data)
#Deskriptif data
summary(data)
describe(data)
hist(data$gizi.buruk)
mean(data$Gizi.Buruk)
var(data$Gizi.Buruk)
length(data$Balita.Gizi.Buruk.Ditemukan..L.P.)
length(data$Bayi.Diberi.ASI.Eksklusif)
#regresi poisson
reg	<- glm(formula=Gizi.Buruk~Bayi.Berat.Badan.Lahir.Rendah+ASI.Eksklusif, family=quasipoisson)
summary(reg)

#regresi binomial negatif
reg	<- glm.nb(formula=Gizi.Buruk~Bayi.Berat.Badan.Lahir.Rendah+ASI.Eksklusif)
summary(reg)
#Linearitas
plot(reg,1)

#Normalitas residual
plot(reg,2)
e	<- reg$residuals
shapiro.test(reg$residuals)
lillie.test(e)

#Autokorelasi 
dwtest(reg)

#homoskedastisitas
bptest(reg, studentize=FALSE)

#Multikolinieritas
vif(reg)

#outlier
plot(reg,5)
plot(reg,4)


