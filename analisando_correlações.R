library(corrplot)

ndados.hw <- as.data.frame(lapply(dados.hw[,4:9], as.numeric))
corrplot(cor(ndados.hw))
ndados.hw <- cbind(dados.hw[,1:3], ndados.hw)
pairs(ndados.hw[, c("cpu", "ram", "disco", "mb_enviados", "mb_recebidos", "processos_ativos")])

#removendo os 100%
ndados.hw.100 <- ndados.hw
ndados.hw.100[ndados.hw.100 == 100] <- NA
hist(ndados.hw.100$disco)

cor(ndados.hw$ram, ndados.hw$processos_ativos)
proc.ram.lm <- lm(processos_ativos ~ ram, data = ndados.hw)
summary(proc.ram.lm)

plot(ndados.hw$ram, ndados.hw$processos_ativos,
     main="Correlação entre a Quantidade de processos ativos e a Ram utilizada", 
     xlab="RAM", ylab="Processos ativos")
abline(proc.ram.lm, lwd=2, col="red")

ram.proc.lm <- lm(ram ~ processos_ativos, data = ndados.hw)
summary(ram.proc.lm)

predict(ram.proc.lm, newdata = data.frame(processos_ativos = c(350)))
