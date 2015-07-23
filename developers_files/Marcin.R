\\ \hline
<<results='asis',echo=FALSE>>=
   m <- numeric(11)
m <- apply(se[,-c(1:3,8,16)], 2, sd)

m <- formatC(m, digits = 2, format = "f")

cat("Odchylenie St&",paste(m, collapse="&") )
@
\\ \hline
<<results='asis',echo=FALSE>>=
   m <- numeric(11)
m <- apply(se[,-c(1:3,8,16)], 2, var)

m <- formatC(m, digits = 2, format = "f")

cat("Wariancja&",paste(m, collapse="&") )
@
\\ \hline
<<results='asis',echo=FALSE>>=
   m <- numeric(11)
m <- apply(se[,-c(1:3,8,16)], 2, med)

m <- formatC(m, digits = 2, format = "f")

cat("Mediana&",paste(m, collapse="&") )
@





