library(bnlearn)
library(Rgraphviz)

dag <- empty.graph(nodes = c("Sexo", "Peso", "Estatura", "Meia"))
dag <- set.arc(dag, from = "Sexo", to = "Peso")
dag <- set.arc(dag, from = "Sexo", to = "Estatura")
dag <- set.arc(dag, from = "Estatura", to = "Meia")
dag
graphviz.plot(dag)

hlight <- list(nodes = nodes(dag), arcs = arcs(dag), col = "grey", textCol = "grey")
pp <- graphviz.plot(dag, highlight = hlight) # returns object of class graph
edgeRenderInfo(pp) <- list(col = c("Estatura~Meia" = "black"),
                           lwd = c("Estatura~Meia" = 3))
nodeRenderInfo(pp) <- list(col = c("Estatura" = "black", "Meia" = "black"),
                           textCol = c("Estatura" = "black", "Meia" = "black"),
                           fill = c("Estatura" = "grey", "Meia" = "yellow"))

renderGraph(pp)
