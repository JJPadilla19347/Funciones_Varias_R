library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(scales)
library(wesanderson)
library(cowplot)
library(foreign)
library(stringr)
library(rlist)
options(repr.plot.width = 2, repr.plot.height = 4)


EDA = function(df, target = "none", print = TRUE, num_lvls = 10){
  
  paleta = scales::hue_pal()(100)
  plots_univar = list()
  
  for (i in 1:ncol(df)) {
    nombre = names(df)[i]
    if (length(unique(df[[i]])) <= num_lvls) {df[[i]] = as.factor(df[[i]])}

    if (is.numeric(df[[i]])){
      # Título
      title <- ggdraw() + 
        draw_label(
          paste("Distribución de", names(df)[i]),
          fontface = 'bold',x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 7))
      
      # Histograma + densidad
      hist = ggplot(df, aes(x = df[[i]])) + 
        geom_density(fill = paleta[i], alpha = 0.5) +
        geom_histogram(aes(y = stat(density)), fill = paleta[i], color = "black", bins = 30) + 
        labs(x = names(df)[i], title = "") +
        theme_light()
      
      
      # Violín + boxplot
      vio = ggplot(df, aes(x = df[[i]])) + 
        geom_violin(fill = paleta[i], aes(y = 1), alpha = 0.5) +
        geom_boxplot(fill = paleta[i], aes(y = 1), width = 0.1) +
        labs(x = names(df)[i], title = "") +
        theme_light()
      
      medio = plot_grid(hist, vio, ncol = 2)
      
      plot = plot_grid(title, medio, nrow = 3, rel_heights = c(0.1, 1))
      if (print) {print(plot)}
      plots_univar = list.append(plots_univar, plot)
      names(plots_univar)[i] = paste0(names(df)[i])

    } else if(is.factor(df[[i]]) | is.logical(df[[i]])){
      
      
      # Transformar df
      temp = df %>%
        mutate(factor = df[[i]]) %>%
        group_by(factor) %>%
        summarise(freq = n(),
                  relativa = freq/nrow(df)) %>%
        ungroup() %>%
        arrange(desc(factor)) %>%
        mutate(pos.relativa = cumsum(relativa) - 0.5 * relativa,
               pos.absoluta = freq * 0.5)
      
      # Gráfico de frecuencias absolutas
      freq = ggplot(temp, aes(x = factor, y = freq, fill = as.factor(factor), label = freq)) +
        geom_bar(stat = "identity") +
        labs(x = "", y = "", 
             title = "",
             subtitle = "Frecuencias absolutas",
             fill = nombre) +
        geom_text(aes(y = freq + 0.05*max(freq)), size = 5, position = position_dodge(width = 0.9), angle = 0) +
        theme_light() +
        theme(legend.position = "none")
      
      # Gráfico de frecuencias relativas
      rel = ggplot(temp, aes(x = 1, y = relativa, fill = as.factor(factor), label = ifelse(relativa >= 0.05, scales::percent(relativa), ""))) +
        geom_bar(position = "stack", stat = "identity") +
        labs(x = "", y = "",
             title = paste(""),
             subtitle = "Frecuencias Relativas",
             fill = nombre) +
        geom_text(aes(y = pos.relativa), size = 5, angle = 0) +
        scale_y_continuous(labels = scales::percent) +
        theme_light() +
        theme(legend.position = "bottom")
      
      # Leyenda  
      legend = get_legend(rel)
      
      # Quitar leyenda de gráficas
      rel = rel + theme(legend.position = "none")
      
      # Título
      title <- ggdraw() + 
        draw_label(
          paste("Distribución de", nombre),
          fontface = 'bold',x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 7))
      
      # Cuerpo
      linea_media = plot_grid(freq, rel, ncol = 2, rel_widths = c(7, 4))
      
      # Mostrar gráfico
      plot = plot_grid(title, linea_media, legend, nrow = 4, rel_heights = c(0.1,1,0.1))
      if (print) {print(plot)}
      plots_univar = list.append(plots_univar, plot)
      names(plots_univar)[i] = paste0(names(df)[i])
    }
  }
    
  
  
  # Análisis multivariado numérico
  
  if (target != "none") {
    plots_multivar = list()
    nombre = target
    numerico = df %>%
      rename("target" = target) %>%
      select(where(is.numeric), target)
    
    for(m_num in 1:(ncol(numerico)-1)){
      plot = ggplot(numerico, aes(y = numerico[[m_num]], x = as.factor(target), fill = as.factor(target))) +
        geom_violin(alpha = 0.5) +
        geom_boxplot(width = 0.1) +
        theme_light() +
        labs(title = paste("Distribución de", names(numerico)[m_num], "por", target),
             y = names(numerico)[m_num], x = nombre, fill = nombre) +
        guides(color = "none", fill = "none")
      plot = plot_grid(plot)
      if (print) {print(plot)}
      plots_multivar = list.append(plots_multivar, plot)
      names(plots_multivar)[m_num] = paste0(names(numerico)[m_num])
    }

    categorico = df %>%
      rename("target" = as.factor(target)) %>%
      select(where(is.factor), target)
    almacen = list()
    
    if (ncol(categorico) > 2){
      for(j in 1:(ncol(categorico)-1)){
        temp = categorico %>%
          mutate(factor = categorico[[j]]) %>%
          group_by(factor) %>%
          mutate(freq_target = n()) %>%
          group_by(factor, target) %>%
          summarise(freq = n(),
                    relativa = freq/freq_target) %>%
          ungroup() %>%
          group_by(factor) %>%
          unique() %>%
          arrange(desc(target)) %>%
          mutate(pos.relativa = cumsum(relativa) - 0.5 * relativa,
                 pos.absoluta = freq * 0.5)
        almacen = list.append(almacen, temp)

      #Gráfico de análisis multivariado de variables categóricas
      plot = ggplot(as.data.frame(almacen[j]), aes(x = as.factor(factor), y = relativa,
                             fill = target,
                             label = ifelse(relativa >= 0.05, scales::percent(relativa), ""))) +
              geom_bar(position = "stack", stat = "identity") +
              labs(x = names(categorico)[j],
                   title = paste("Frecuencias relativas de", target,"por", names(categorico)[j]),
                   fill = target) +
              geom_text(aes(y = pos.relativa), size = 5, angle = 0) +
              theme_light() +
              theme(legend.position = "bottom")
      
      plots_multivar = list.append(plots_multivar, plot)
      names(plots_multivar)[m_num + j] = paste0(names(categorico)[j])
      if (print) {print(plot)}
      }
    }
  }
  if(target == "none"){
    plots = plots_univar
  } else  plots = list("Univar" = plots_univar, "Multivar" = plots_multivar)
 
  return(plots)  
  }
