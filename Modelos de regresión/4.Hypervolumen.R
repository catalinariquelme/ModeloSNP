library("ecr")
library(readxl)
#cat("\014")

modelo_base <- read.csv("C:/Users/ca_re/OneDrive/Desktop/Modelos/Codigo supremo v2/Información/ModelosBase.csv")
modelo_incremental <- read.csv("C:/Users/ca_re/OneDrive/Desktop/Modelos/Codigo supremo v2/Información/ModelosIncremental.csv")
modelo_cada_gen <- read.csv("C:/Users/ca_re/OneDrive/Desktop/Modelos/Codigo supremo v2/Información/ModelosPorGen.csv")
fenotipos_path <- "C:/Users/ca_re/OneDrive/Desktop/Modelos/fenotipos.xlsx"
lectura_datos <- read_excel(fenotipos_path, 
                            col_types = c(rep("text", 55), rep("numeric", 12)),
                            na = "NA")
nombres_fenotipos <- names(lectura_datos)[56:67]


fenotipo <- "SM60_Rate" # Nombre del fenotipo a analizar
print(fenotipo)

# Se obtiene la información por cada fenotipo
mod_gen_fil <- modelo_cada_gen[modelo_cada_gen$Fenotipo == fenotipo, ]
mod_incremental_fil <- modelo_incremental[modelo_incremental$Fenotipo == fenotipo, ]
mod_base_fil <- modelo_base[modelo_base$Fenotipo == fenotipo, ]

# Se juntan los modelos base, por SNP e incremental del fenotipo
modelos <- rbind(mod_gen_fil,mod_incremental_fil,mod_base_fil)
modelos <- modelos[-which(is.na(modelos$R2)),] 
modelos_re <- modelos 

# R2 -> 1 - R2
modelos$R2 <- 1 - modelos$R2

prueba <- cbind(modelos$MSE,modelos$R2)
colnames(prueba) = c("MSE","1-R2")
row.names(prueba) <- modelos$Modelo

# Se normalizan los datos
for (a in 1:ncol(prueba)){
  prueba[,a] = (prueba[,a]-min(prueba[,a]))/(max(prueba[,a])-min(prueba[,a]))
}

# HYPERVOLUMEN
resultado=computeHVContr(t(prueba), ref.point = c(2,2))

# Se obtiene el nombre del modelo con mayor hypervolumen
indice_maximo = which.max(resultado)
final <- (rownames(prueba)[row(prueba) == indice_maximo])[1]
print(final)

print(subset(modelos_re, Modelo == final))


