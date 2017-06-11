library(rvest)
library(dplyr)
library(stringr)
page <- read_html("http://www.mifolkloreargentino.com.ar/letras-de-canciones")

# Obtener lista de Artistas
artistas <- page %>% 
  html_nodes(css = 'div[role="tabpanel"] > div > div > div[class="media-left"] > a > img') %>%
  html_attr("alt") %>%
  str_trim("both")

# Obtener lista de Links a la Pagina de los Artistas
links <- page %>% 
  html_nodes(css = 'div[role="tabpanel"] > div > div > div[class="media-left"] > a') %>%
  html_attr("href")

artistas <- data.frame(artista = artistas, 
                       link = links, 
                       stringsAsFactors = FALSE)

# Obtener links a cada una de las canciones de cada Artista
linksCanciones <- data.frame(artista = character(), 
                             nombre_cancion = character(),
                             link_cancion = character())

for (i in 1:nrow(artistas)) {
  
  artistPage <- read_html(artistas$link[i])

  nombreCancion <- artistPage %>%
    html_nodes("body > div.container > div > div > div > div.col-xs-12.col-sm-8.col-md-9 > section > div > div > article > h3 > a") %>%
    html_text() %>%
    str_trim("both")
  
  linkCancion <- artistPage %>%
    html_nodes("body > div.container > div > div > div > div.col-xs-12.col-sm-8.col-md-9 > section > div > div > article > h3 > a") %>%
    html_attr("href")
  
  canciones <- data.frame(artista = artistas$artista[i],
                          nombre_cancion = nombreCancion,
                          link_cancion = linkCancion,
                          stringsAsFactors = FALSE)
  
  linksCanciones <- rbind(linksCanciones, canciones)
  print(sprintf("Terminado Artista %s. Links a canciones scrapeados: %i.",
                artistas$artista[i],
                nrow(linksCanciones)))
}


# Obtener letra de cada una de las canciones de un artista
# http://www.mifolkloreargentino.com.ar/letras-de-canciones-de-abel-pintos/a-dios

# Funcion para eliminar todos los tags del Texto
# cleanFun <- function(htmlString) {
#   return(gsub("<.*?>", "", htmlString))
# }

letrasCanciones <- character()

print("Comenzando a Scrapear letras de canciones.")
for (i in 1:nrow(linksCanciones)) {
    url <- linksCanciones$link_cancion[i]
    paginaCancion <- tryCatch(
      {
        read_html(url)
      },
      error=function(cond) {
        message(paste("URL does not seem to exist:", url))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      },
      warning=function(cond) {
        message(paste("URL caused a warning:", url))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NA)
      },
      finally={
        #message(paste("Processed URL:", url))
      }
    )
  
  if(!is.na(paginaCancion)){
    letra <- paginaCancion %>% 
      html_node("div.article-post-content") %>%
      html_text() %>%
      str_trim("both") %>%
      gsub("\n","",.) %>% 
      gsub("\t","",.)
    
  } else {
    letra <- "LETRA NOT FOUND HTTP 400"
  }

  letrasCanciones <- c(letrasCanciones, letra)
  if(i %% 100 == 0){
    print(sprintf("Finalizada letra: %i de %i (%i%%)", 
                  i, 
                  nrow(linksCanciones),
                  as.integer((i / nrow(linksCanciones) * 100 ))))  
  }
  
}

datosCanciones <- data.frame(linksCanciones, 
                             letra = letrasCanciones,
                             stringsAsFactors = FALSE)

# Limpieza de datos
datosCanciones <- datosCanciones %>%
  filter(!is.na(letra),
         letra != "LETRA NOT FOUND HTTP 400",
         letra != "...")

readr::write_csv(datosCanciones, path = "./datosCanciones.csv")
