library(dplyr)
library(ggplot2)
library(reshape2)

jub <- read.csv("haber_medio_sexo.csv")
jub$Fecha <- as.Date(paste0(jub$Anio,formatC(jub$Mes,width = 2,flag=0),"01"), format="%Y%m%d")

jub$Relacion <- (jub$Haber.Masculino / jub$Haber.Femenino) - 1

jub.melt <- melt(jub[,c("Fecha", "Haber.Femenino", "Haber.Masculino")], id.vars = "Fecha")

p1 <- ggplot(jub.melt, aes(x=Fecha, y=value, colour=variable)) + 
    geom_line() +
    theme_minimal()  +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = NULL) +
    labs(title="Jubilación Promedio por Sexo en Córdoba",
         subtitle="Haber Jubilatorio Medio de los beneficiarios que perciben sus haberes en la ciudad de Córdoba según sexo.",
         caption = "Fuente: Open Data Cordoba con datos de https://gobiernoabierto.cordoba.gob.ar")
ggsave("haber-promedio-por-sexo.jpg", width = 11, height = 7)

p2 <- ggplot(jub, aes(x=Fecha, y=Relacion)) + 
    geom_line() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = NULL) +
    labs(title="Evolución de la Diferencia en los Haberes Promedio Masculinos y Femeninos",
         subtitle="Porcentaje de Diferencia en el Haber Medio Masculino vs Femenino para la Ciudad de Córdoba. ¿Cuánto más cobra un Hombre que una Mujer?",
         caption = "Fuente: Open Data Cordoba con datos de https://gobiernoabierto.cordoba.gob.ar")
ggsave("evolucion-diferencia-con-outlier.jpg", width = 11, height = 7)

# Suavizado de la Curva mediante el reemplazo del Outlier por el valor más cercano
jub[which.min(jub$Relacion),"Haber.Femenino"] <- jub[which.min(jub$Relacion)-1,"Haber.Femenino"]
# Recalculo de la Relacion
jub$Relacion <- (jub$Haber.Masculino / jub$Haber.Femenino) - 1

p3 <- ggplot(jub, aes(x=Fecha, y=Relacion)) + 
    geom_line() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent, limits = c(.15,.45), breaks = seq(.15,.45,.05), minor_breaks = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", minor_breaks = NULL) +
    labs(title="Evolución en la Diferencia en los Haberes Promedio Masculinos y Femeninos",
         subtitle="Porcentaje de Diferencia en el Haber Jubilatorio Medio Masculino vs Femenino para la Ciudad de Córdoba. ¿Cuánto más cobra un Hombre que una Mujer?",
         caption = "Fuente: Open Data Cordoba con datos de https://gobiernoabierto.cordoba.gob.ar")
ggsave("evolucion-diferencia-sin-outlier.jpg", width = 11, height = 7)
