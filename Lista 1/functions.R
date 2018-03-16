PrintGrafs = function(x)
{
  
  # Define o nome do arquivo
  FileName = paste("./Lista 1/Output/plot p_", p, ".pdf", sep="")
  
  #  inicia o obj de grafico
  pdf(file=FileName)
  
  
  for(i in 1:ncol(x))
  {
    nome = paste("Histograma ", colnames(x)[i], sep = "")
    hist(x[,i],freq = F, labels = F, main=nome,  xlab = "Media", breaks = 20)
    lines(density(x[,i], adjust = 2), col = "Red", lwd=2)
  }
  dev.off()
}
