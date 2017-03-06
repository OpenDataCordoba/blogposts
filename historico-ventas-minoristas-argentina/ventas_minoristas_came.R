library(ggplot2)

ventas <- read.csv("./data/ventas_minoristas_came.csv")
ventas$Mes <- as.Date(ventas$Mes, format="%d-%m-%Y")
ventas$Variacion.Interanual <- ventas$Variacion.Interanual / 100

# Feo pero comodo
anotaciones <- data.frame(fecha = "2011-12-10", label="Cristina Fernandez de Kirchner")
anotaciones <- rbind(anotaciones, data.frame(fecha = "2015-12-10", label="Mauricio Macri"))
anotaciones$fecha <- as.Date(anotaciones$fecha, format="%Y-%m-%d")

ggplot(ventas, aes(x=Mes, y=Variacion.Interanual)) + 
    geom_col() +
    theme_minimal() + 
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
    scale_y_continuous(labels=scales::percent) +
    labs(title = "Ventas Minoristas Argentina",
         subtitle = "Histórico de la Variación Interanual de Ventas Minoristas mes a mes según CAME.",
         caption = "Relevamiento: Open Data Cordoba.") +
    ylab("Variación Interanual") +
    xlab("Fecha") +
    geom_vline(xintercept=as.numeric(anotaciones$fecha), linetype=3) +
    geom_text(data=anotaciones,mapping=aes(x=fecha, y=-0.1, label=label),
              angle=90, vjust=-1, size=3.5, fontface='italic')
    