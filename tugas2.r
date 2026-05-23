library(readxl)

data <- read_excel("C:/Users/Downloads/data_pendapatan.xlsx")

ubah kolom kedua dan ketiga menjadi pendapatan dan kebahagiaan

names (data)[2:3] <- c("pendapatan","kebahagiaan")

view(data)

data$kebahagiaan <- as.numeric(data$kebahagiaan)

hist(data$kebahagiaan,
main = "Histogram kebahagiaan"
xlab = "Kebahagiaan",
ylab = "Frekuensi",
col = "skyblue",
border = "black")

data$pendapatan <- as.numeric(data$pendapatan)
plot(data$pendapatan, data$kebahagiaan, 
main = "Scatter Plot pendapatan & kebahagiaan",
xlab = "Pendapatan",
ylab = "Nilai kebahagiaan",
pch = 19,
col = "red")

model <- lm(kebahagiaan ~ pendapatan, data = data)

abline(model, col="blue", lwd=2)

model <- lm(kebahagiaan ~ pendapatan, data = data)
summary(model)

par(mfrow=c(2,2))
plot(model)

par(mfrow=c(1, 1))

plot(data$pendapatan, data$kebahagiaan,
     main="Regresi Linear: Pengaruh Pendapatan terhadap Kebahagiaan",
     xlab="Pendapatan",
     ylab="Kebahagiaan",
     col="red",
     pch=19)

abline(model, col="black", lwd=2)

coef_model <- coef(model)

eq <- paste("y =", round(coef_model[1], 3), "+",
            round(coef_model[2], 3), "x")

r2 <- paste("R2 =", round(summary(model)$r.squared, 3))

text(x=min(data$pendapatan),
     y=max(data$kebahagiaan),
     labels=eq,
     pos=4,
     col="red")

text(x=min(data$pendapatan),
     y=max(data$kebahagiaan)-0.5,
     labels=r2,
     pos=4,
     col="red")
