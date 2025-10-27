install.packages("aws.s3")
library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "",
  "AWS_SECRET_ACCESS_KEY" = "",
  "AWS_SESSION_TOKEN" = "",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

bucket.hardware <- get_bucket("raw-paymetrics", prefix = "hardware/", max = Inf)

dados.hw <- data.frame(
  datetime = as.POSIXct(character()),
  codigo_maquina = character(),
  mac_address = character(),
  cpu = numeric(),
  ram = numeric(),
  disco = numeric(),
  mb_enviados = numeric(),
  mb_recebidos = numeric(),
  processos_ativos = integer(),
  stringsAsFactors = FALSE
)

for (i in 1:length(bucket.hardware)) {
  key <- bucket.hardware[[i]]$Key
  print(key)
  
  if (endsWith(key, "/")) next
  
  df <- s3read_using(
    FUN = read.csv2,
    bucket = "raw-paymetrics",
    object = key
  )
  
  dados.hw <- rbind(dados.hw, df)
  rm(df)
}

dados.hw$codigo_maquina[dados.hw$codigo_maquina == "CD002"] <- "COD002"
