library("tidyr")
library("ggplot2")

str(ndados.hw)


# analisando metricas -----------------------------------------------------

summary(ndados.hw$cpu)
summary(ndados.hw$ram)
summary(ndados.hw$disco)
summary(ndados.hw$mb_recebidos)
summary(ndados.hw$mb_enviados)


# analisando por tempo ----------------------------------------------------
summary(ndados.hw$datetime)

ndados.hw$datetime <- as.POSIXct(ndados.hw$datetime, tz = "UTC")

dia.especifico.cod002 <- ndados.hw[as.Date(ndados.hw$datetime) == as.Date("2025-10-17") & ndados.hw$codigo_maquina == "COD002", ]
plot(dia.especifico.cod002$datetime, 
     dia.especifico.cod002$cpu, 
     type = "l", col="blue", lwd=2,
     main="Variação da CPU da máquina 'COD002' no dia 17/10/2025",
     xlab = "Horário", ylab="CPU")

plot(dia.especifico.cod002$datetime, 
     dia.especifico.cod002$ram, 
     type = "l", col="blue", lwd=2,
     main="Variação da RAM da máquina 'COD002' no dia 17/10/2025",
     xlab = "Horário", ylab="RAM")

plot(dia.especifico.cod002$datetime, 
     dia.especifico.cod002$disco, 
     type = "l", col="blue", lwd=2,
     main="Variação de DISCO da máquina 'COD002' no dia 17/10/2025",
     xlab = "Horário", ylab="Disco")

plot(dia.especifico.cod002$datetime, 
     dia.especifico.cod002$mb_enviados, 
     type = "l", col="blue", lwd=2,
     main="Variação de MB Enviados da máquina 'COD002' no dia 17/10/2025",
     xlab = "Horário", ylab="MB Enviados")

plot(dia.especifico.cod002$datetime, 
     dia.especifico.cod002$mb_recebidos, 
     type = "l", col="blue", lwd=2,
     main="Variação de MB Recebidos da máquina 'COD002' no dia 17/10/2025",
     xlab = "Horário", ylab="MB Recebidos")

plot(dia.especifico.cod002$datetime, 
     dia.especifico.cod002$processos_ativos, 
     type = "l", col="blue", lwd=2,
     main="Variação de Processos Ativos da máquina 'COD002' no dia 17/10/2025",
     xlab = "Horário", ylab="Processos Ativos")


dia.especifico.cod003 <- ndados.hw[as.Date(ndados.hw$datetime) == as.Date("2025-10-18") & ndados.hw$codigo_maquina == "COD003", ]
plot(dia.especifico.cod003$datetime, 
     dia.especifico.cod003$cpu, 
     type = "l", col="red", lwd=2,
     main="Variação da CPU da máquina 'COD003' no dia 18/10/2025",
     xlab = "Horário", ylab="CPU")

plot(dia.especifico.cod003$datetime, 
     dia.especifico.cod003$ram, 
     type = "l", col="red", lwd=2,
     main="Variação da RAM da máquina 'COD003' no dia 18/10/2025",
     xlab = "Horário", ylab="RAM")

plot(dia.especifico.cod003$datetime, 
     dia.especifico.cod003$disco, 
     type = "l", col="red", lwd=2,
     main="Variação de DISCO da máquina 'COD003' no dia 18/10/2025",
     xlab = "Horário", ylab="Disco")

plot(dia.especifico.cod003$datetime, 
     dia.especifico.cod003$mb_enviados, 
     type = "l", col="red", lwd=2,
     main="Variação de MB Enviados da máquina 'COD003' no dia 18/10/2025",
     xlab = "Horário", ylab="MB Enviados")

plot(dia.especifico.cod003$datetime, 
     dia.especifico.cod003$mb_recebidos, 
     type = "l", col="red", lwd=2,
     main="Variação de MB Recebidos da máquina 'COD003' no dia 18/10/2025",
     xlab = "Horário", ylab="MB Recebidos")

plot(dia.especifico.cod003$datetime, 
     dia.especifico.cod003$processos_ativos, 
     type = "l", col="red", lwd=2,
     main="Variação de Processos Ativos da máquina 'COD003' no dia 18/10/2025",
     xlab = "Horário", ylab="Processos Ativos")


# analisar por maquina ----------------------------------------------------

# cpu
dados.hw.piv <- pivot_longer(ndados.hw, 
                             cols = c("cpu", "ram", "disco", "mb_enviados", "mb_recebidos", "processos_ativos"),
                             values_to = "medicao",
                             names_to = "tipo_medicao")
ggplot(mapping = aes(dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "cpu"], 
                     dados.hw.piv$medicao[dados.hw.piv$tipo_medicao == "cpu"],
                     fill = dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "cpu"]
                     ))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title = "Uso de CPU nas máquinas cadastradas",
    x="Máquinas cadastradas",
    y="Uso de CPU (%)"
  )+
  scale_fill_discrete(name = "Máquinas cadastradas")

# ram
ggplot(mapping = aes(dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "ram"], 
                     dados.hw.piv$medicao[dados.hw.piv$tipo_medicao == "ram"],
                     fill = dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "ram"]
))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title = "Uso de RAM nas máquinas cadastradas",
    x="Máquinas cadastradas",
    y="Uso de RAM (%)"
  )+
  scale_fill_discrete(name = "Máquinas cadastradas")


# disco
ggplot(mapping = aes(dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "disco"], 
                     dados.hw.piv$medicao[dados.hw.piv$tipo_medicao == "disco"],
                     fill = dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "disco"]
))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title = "Uso de DISCO nas máquinas cadastradas",
    x="Máquinas cadastradas",
    y="Uso de DISCO (%)"
  )+
  scale_fill_discrete(name = "Máquinas cadastradas")


# mb enviados  
ggplot(mapping = aes(dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "mb_enviados"], 
                     dados.hw.piv$medicao[dados.hw.piv$tipo_medicao == "mb_enviados"],
                     fill = dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "mb_enviados"]
))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title = "MB Enviados nas máquinas cadastradas",
    x="Máquinas cadastradas",
    y="MB Enviados"
  )+
  scale_fill_discrete(name = "Máquinas cadastradas")

# mb recebidos  
ggplot(mapping = aes(dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "mb_recebidos"], 
                     dados.hw.piv$medicao[dados.hw.piv$tipo_medicao == "mb_recebidos"],
                     fill = dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "mb_recebidos"]
))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title = "MB Recebidos nas máquinas cadastradas",
    x="Máquinas cadastradas",
    y="MB Recebidos"
  )+
  scale_fill_discrete(name = "Máquinas cadastradas")

# processos ativos
ggplot(mapping = aes(dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "processos_ativos"], 
                     dados.hw.piv$medicao[dados.hw.piv$tipo_medicao == "processos_ativos"],
                     fill = dados.hw.piv$codigo_maquina[dados.hw.piv$tipo_medicao == "processos_ativos"]
))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title = "Processos ativos nas máquinas cadastradas",
    x="Máquinas cadastradas",
    y="Processos ativos"
  )+
  scale_fill_discrete(name = "Máquinas cadastradas")




# analisar metricas -------------------------------------------------------

q92.cpu <- quantile(ndados.hw$cpu, 0.99, na.rm = TRUE)
q92.cpu # 70.25752
hist(ndados.hw$cpu)

q90.ram <- quantile(ndados.hw.100$ram, 0.90, na.rm = TRUE)
q90.ram # 95.10721
hist(ndados.hw$ram)

q90.disco <- quantile(ndados.hw.100$disco, 0.90, na.rm = TRUE)
q90.disco # 84.66333
hist(ndados.hw$disco)

q90.mb_enviados <- quantile(ndados.hw.100$mb_enviados, 0.90, na.rm = TRUE)
q90.mb_enviados # 0.2
hist(ndados.hw$mb_enviados)

q90.mb_recebidos <- quantile(ndados.hw.100$mb_recebidos, 0.90, na.rm = TRUE)
q90.mb_recebidos # 1.7446 
hist(ndados.hw$mb_recebidos)

q95.cpu <- quantile(ndados.hw$cpu, 0.95, na.rm = TRUE)
q95.cpu # 95.42518 
hist(ndados.hw$cpu)

q95.ram <- quantile(ndados.hw.100$ram, 0.95, na.rm = TRUE)
q95.ram # 97.76047
hist(ndados.hw$ram)

q95.disco <- quantile(ndados.hw.100$disco, 0.95, na.rm = TRUE)
q95.disco # 90.58867
hist(ndados.hw$disco)

q95.mb_enviados <- quantile(ndados.hw.100$mb_enviados, 0.95, na.rm = TRUE)
q95.mb_enviados # 0.3891
hist(ndados.hw$mb_enviados)

q95.mb_recebidos <- quantile(ndados.hw.100$mb_recebidos, 0.95, na.rm = TRUE)
q95.mb_recebidos # 5.8297
hist(ndados.hw$mb_recebidos)

