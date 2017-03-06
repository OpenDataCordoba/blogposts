library(ggplot2)

ventas <- read.csv("ventas_minoristas_came.csv")
ventas$Mes <- as.Date(ventas$Mes, format="%d-%m-%Y")
ventas$Variacion.Interanual <- ventas$Variacion.Interanual / 100

ggplot(ventas, aes(x=Mes, y=Variacion.Interanual)) + 
    geom_col() +
    theme_minimal() + 
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
    scale_y_continuous(labels=scales::percent, breaks=seq(-0.2,0.18,.02), minor_breaks = NULL) +
    labs(title = "Ventas Minoristas Argentina",
         subtitle = "Histórico de la Variación Interanual de Ventas Minoristas mes a mes según CAME.",
         caption = "Relevamiento de Información: Open Data Córdoba en base a publicaciones de Red CAME.") +
    ylab("Variación Interanual") +
    xlab("Fecha")

ggsave("ventas-minoristas-came.jpg", width = 10, height = 7)
