library(tmap)
library(giscoR)
library(mapSpain)
library(sf)
library(ggplot2)
library(tmap)
library(leaflet)
library(viridis)
library(ggspatial)
library(stringdist)
library(readr)
library(dplyr)


catalunya <- st_read("Catalunya.geojson", quiet = TRUE)
provincies <- st_read("Provincies.geojson", quiet = TRUE)
vegueries <- st_read("Vegueries.geojson", quiet = TRUE)
comarques <- st_read("Comarques.geojson", quiet = TRUE)
municipis <- st_read("Municipis2.geojson", quiet = TRUE)
seccions_censals <- st_read("SeccionsCensals2.geojson", quiet = TRUE)

Resultats_Seccions <- read_csv("Resultats_Seccions.csv")
Resultats_Renda <- read_csv("Resultats_Renda.csv")
Renda_Seccions <- read_csv("Renda_Seccions.csv")

colors_partits <- c(
  "PSC" = "#FF0000",        
  "CAT-JUNTS+" = "#00C3B2", 
  "PP" = "#0198CB",         
  "ERC" = "yellow",        
  "VOX" = "#63BE21",        
  "NA" = "#808080",
  "NULL" = "#808080",
  "ALIANÇA.CAT" = "#064a81"
)

provincies <- provincies %>% 
  rename(
    Provincia = NOMPROV,
  )

comarques <- comarques %>% rename(
  Comarca = NOMCOMAR,
  Capital = CAPCOMAR
)

vegueries <- vegueries %>% 
  rename(
  Vegueria = NOMVEGUE,
  Capital = CAPVEGUE
)


cat_interactive <- function(shp, color = "black", fill_color = NA, opacity = 1, nom = "Mapa", popup = NULL, 
                            data = NULL, variable = NULL, gradient = c("white", "blue"), 
                            variable_categor = NULL, categor_colors = NULL, gradient_variable = NULL,
                            percentage_divisor = NULL, n_breaks = 7, style = "quantile", ...) {

  if (!inherits(shp, "sf")) {
    stop("El objeto 'shp' no es un objeto sf válido.")
  }
  
  tmap_mode("view")
  
  filtros <- list(...)
  
  if ("Secció" %in% colnames(shp) & "Districte" %in% colnames(shp)) {
    nivel_uso <- "Secció Censal"
    col_agrupacion <- c("Secció", "Districte", "Municipi")
  } else if ("Municipi" %in% colnames(shp)) {
    nivel_uso <- "Municipi"
    col_agrupacion <- "Municipi"
  } else if ("Comarca" %in% colnames(shp)) {
    nivel_uso <- "Comarca"
    col_agrupacion <- "Comarca"
  } else if ("Vegueria" %in% colnames(shp)) {
    nivel_uso <- "Vegueria"
    col_agrupacion <- "Vegueria"
  } else if ("Provincia" %in% colnames(shp)) {
    nivel_uso <- "Provincia"
    col_agrupacion <- "Provincia"
  } else if ("Catalunya" %in% colnames(shp)) {
    nivel_uso <- "Catalunya"
    col_agrupacion <- "Catalunya"
  } else {
    stop("No se pudo determinar el nivel de agregación del shapefile.")
  }
  
  cat("Nivel de agregación utilizado:", nivel_uso, "\n")  
  
  if (length(filtros) > 0) {
    for (var in names(filtros)) {
      if (var %in% colnames(shp)) {
        shp <- shp[shp[[var]] %in% filtros[[var]], ]  
      } else {
        warning(paste("La variable", var, "no existe en el dataset."))
      }
    }
  }
  
  if (!is.null(data)) {
    if (!is.null(variable)) {
      if (variable %in% colnames(data)) {
        
        if (!all(col_agrupacion %in% colnames(data))) {
          stop(paste("Las columnas de agrupación", paste(col_agrupacion, collapse = ", "), "no existen en 'data'."))
        }
        
        if (length(col_agrupacion) > 1) {
          data_agg <- aggregate(data[, c(variable, percentage_divisor), drop = FALSE], 
                                by = lapply(col_agrupacion, function(col) data[[col]]), 
                                sum, na.rm = TRUE)
          colnames(data_agg)[1:length(col_agrupacion)] <- col_agrupacion  
        } else {
          data_agg <- aggregate(data[, c(variable, percentage_divisor), drop = FALSE], 
                                by = list(data[[col_agrupacion]]), sum, na.rm = TRUE)
          colnames(data_agg)[1] <- col_agrupacion  
        }
        
        if (!is.null(percentage_divisor)) {
          data_agg$percentage <- round((data_agg[[variable]] / data_agg[[percentage_divisor]]) * 100, 2)
        }
        
        shp <- merge(shp, data_agg, by = col_agrupacion, all.x = TRUE)
        
      } else {
        stop(paste("La variable", variable, "no existe en el DataFrame 'data'."))
      }
    }
    
    if (!is.null(variable_categor)) {
      if (variable_categor %in% colnames(data)) {
        
        if (!all(col_agrupacion %in% colnames(data))) {
          stop(paste("Las columnas de agrupación", paste(col_agrupacion, collapse = ", "), "no existen en 'data'."))
        }
        
        if (length(col_agrupacion) > 1) {
          data_agg_categor <- aggregate(data[, variable_categor, drop = FALSE], 
                                        by = lapply(col_agrupacion, function(col) data[[col]]), 
                                        function(x) names(sort(table(x), decreasing = TRUE))[1])
          colnames(data_agg_categor)[1:length(col_agrupacion)] <- col_agrupacion  
        } else {
          data_agg_categor <- aggregate(data[, variable_categor, drop = FALSE], 
                                        by = list(data[[col_agrupacion]]), 
                                        function(x) names(sort(table(x), decreasing = TRUE))[1])
          colnames(data_agg_categor)[1] <- col_agrupacion  
        }
        
        data_agg_categor[[variable_categor]] <- as.character(data_agg_categor[[variable_categor]])
        
        shp <- merge(shp, data_agg_categor, by = col_agrupacion, all.x = TRUE)
        
      } else {
        stop(paste("La variable categórica", variable_categor, "no existe en el DataFrame 'data'."))
      }
    }
    
    if (!is.null(gradient_variable)) {
      if (gradient_variable %in% colnames(data)) {
        
        if (length(col_agrupacion) > 1) {
          data_agg_gradient <- aggregate(data[, gradient_variable, drop = FALSE], 
                                         by = lapply(col_agrupacion, function(col) data[[col]]), 
                                         sum, na.rm = TRUE)
          colnames(data_agg_gradient)[1:length(col_agrupacion)] <- col_agrupacion 
        } else {
          data_agg_gradient <- aggregate(data[, gradient_variable, drop = FALSE], 
                                         by = list(data[[col_agrupacion]]), 
                                         sum, na.rm = TRUE)
          colnames(data_agg_gradient)[1] <- col_agrupacion  
        }
        
        shp <- merge(shp, data_agg_gradient, by = col_agrupacion, all.x = TRUE)
        
      } else {
        stop(paste("La variable de gradiente", gradient_variable, "no existe en el DataFrame 'data'."))
      }
    }
  }
  

  if (!is.null(popup)) {
    popup_existentes <- popup[popup %in% colnames(shp)]
    if (length(popup_existentes) > 0) {
      shp$popup_text <- apply(st_drop_geometry(shp[popup_existentes]), 1, function(row) {
        paste(row, collapse = " | ") 
      })
    } else {
      shp$popup_text <- NULL
    }
    
    if ("percentage" %in% colnames(shp)) {
      shp$popup_text <- paste(shp$popup_text, paste("Porcentaje:", shp$percentage, "%"), sep = " | ")
    }
  } else {
    shp$popup_text <- NULL
  }
  
  tm <- tm_shape(shp) +
    tm_basemap(server = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"))
  
  if (!is.null(variable)) {
    tm <- tm +
      tm_polygons(
        col = ifelse(!is.null(percentage_divisor), "percentage", 
                     ifelse(!is.null(variable), variable, fill_color)), 
        palette = gradient, 
        alpha = opacity, 
        border.col = color, 
        id = "popup_text",
        style = style,  
        n = n_breaks  
      )
  }
  
  if (!is.null(variable_categor)) {
    if (is.null(categor_colors)) {
      categor_colors <- rainbow(length(unique(shp[[variable_categor]])))
    }
    
    if (!is.null(gradient_variable)) {
      tm <- tm +
        tm_polygons(
          col = variable_categor, 
          palette = categor_colors, 
          alpha = opacity, 
          border.col = color, 
          id = "popup_text",
          style = style,  
          n = n_breaks,  
          legend.col.show = FALSE  
        ) +
        tm_fill(
          col = gradient_variable, 
          palette = gradient, 
          alpha = opacity, 
          style = style, 
          n = n_breaks  
        )
    } else {
      tm <- tm +
        tm_polygons(
          col = variable_categor, 
          palette = categor_colors, 
          alpha = opacity, 
          border.col = color, 
          id = "popup_text"
        )
    }
  }
  
  tm <- tm +
    tm_layout(title = nom, frame = FALSE)
  
  tm_leaflet <- tmap_leaflet(tm)
  
 
  tm_leaflet <- tm_leaflet %>%
    htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      
      
      var slider = L.control({position: 'topright'});
      
      slider.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'slider-control');
        div.style.padding = '10px';  
        div.style.backgroundColor = 'white'; 
        div.style.borderRadius = '5px'; 
        div.style.boxShadow = '0 0 5px rgba(0,0,0,0.3)';  
        div.innerHTML = '<input type=\"range\" min=\"0\" max=\"1\" step=\"0.1\" value=\"' + x.options.opacity + '\">';
        
        div.addEventListener('mouseenter', function() {
          map.dragging.disable();
        });
        
        div.addEventListener('mouseleave', function() {
          map.dragging.enable();
        });
        
        return div;
      };
      
      slider.addTo(map);

      var fullscreenButton = L.control({position: 'topright'});
      
      fullscreenButton.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'fullscreen-control');
        div.style.padding = '5px';  
        div.style.backgroundColor = 'white';  
        div.style.borderRadius = '5px';  
        div.style.boxShadow = '0 0 5px rgba(0,0,0,0.3)';  
        div.innerHTML = '<button style=\"border: none; background: none; cursor: pointer;\">⛶</button>';  /
        return div;
      };
      
      fullscreenButton.addTo(map);
      
      var fullscreenButtonElement = document.querySelector('.fullscreen-control button');
      fullscreenButtonElement.addEventListener('click', function() {
        if (map._container.requestFullscreen) {
          map._container.requestFullscreen();
        } else if (map._container.mozRequestFullScreen) {  
          map._container.mozRequestFullScreen();
        } else if (map._container.webkitRequestFullscreen) {  
          map._container.webkitRequestFullscreen();
        } else if (map._container.msRequestFullscreen) {  
          map._container.msRequestFullscreen();
        }
      });
      
      var input = document.querySelector('.slider-control input');
      input.addEventListener('input', function() {
        var opacity = parseFloat(this.value);
        map.eachLayer(function(layer) {
          if (layer.setStyle) {
            layer.setStyle({fillOpacity: opacity});
          }
        });
      });
    }
  ")
  
  return(tm_leaflet)
}


## Ejemplo 1:

cat_interactive(seccions_censals,
                Municipi = "Terrassa",
                nom = "Porcentaje de votos a PSC en Terrassa por secciones censales",
                data = Resultats_Seccions,
                variable = "PSC Vots",
                percentage_divisor = "Vots vàlids",
                gradient = c("white", "#ff0000"),
                style = "cont",
                opacity = 1,
                popup = c("Districte","Secció"))

## Ejemplo 2: 

cat_interactive(municipis,
                nom = "Partido ganador por municipio",
                data = Resultats_Seccions,
                variable_categor = "Guanyador",
                categor_colors = colors_partits,
                opacity = 1,
                popup = c("Municipi","Guanyador"))
