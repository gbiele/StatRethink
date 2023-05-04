library(magrittr)
library(dagitty)
library(ggdag)
library(ggplot2)
library(data.table)
library(patchwork)

plot_dag = function(dag, curves = T) {
  
  tdag = 
    data.table(tidy_dagitty(dag)$data) %>% 
    .[!is.na(direction), curvature := 0] %>% 
    .[name == "C" & to %in% paste0("I",c("n",2:4)), curvature := ifelse(curves == T,  1, 2)] %>% 
    .[name == "C" & to %in% paste0("D",c("n",2:4)), curvature := ifelse(curves == T, 1, 0)] %>% 
    .[name == "E" & to == "N", curvature := ifelse(curves == T, .33, 0)] %>% 
    .[name == "N" & to == "S", curvature := ifelse(curves == T, .33, 0)] %>% 
    .[, txt.clr := "black"] %>% 
    .[grep("b[0-9]",name), name := "b"]
  
  
  g = ggplot(tdag,
             aes(x = x, y = y,
                 xend = xend, yend = yend)) + 
    theme_dag() + 
    theme(panel.background = element_rect(fill = "white", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_dag_point(col = "white", show.legend = F) +
    geom_point(data = tdag[grepl("S$|S[0-9]$|C",name)],pch = 1, size = 10) +
    geom_point(data = tdag[grepl("U$|U[0-9]$|[U,I,F]C",name)],pch = 16, size = 10, color = "grey") +
    geom_dag_text(aes(color = txt.clr),show.legend = F) +
    scale_color_manual(values=c("black", "grey")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  my.arrow = grid::arrow(length = grid::unit(7, "pt"), type = "open")
  if (any(tdag$curvature != 0,na.rm = T)) {
    g = g + geom_dag_edges_arc(
      curvature = tdag[!is.na(direction),curvature], arrow = my.arrow)
  } else {
    g = g + geom_dag_edges(arrow_directed = my.arrow)
  }
  
  
  
  adjusted_vars = sapply(grep("adjusted",
                              strsplit(dag,"\n")[[1]],
                              value = T),
                         function(x)
                           strsplit(x," ")[[1]][1])
  
  for (v in adjusted_vars) {
    g = g + geom_point(data = tdag[name == v],
                       pch = 1, size = ifelse(nchar(v)<3,10,11))
  }
  
  g$coordinates$clip = "off"
  return(g)
}
