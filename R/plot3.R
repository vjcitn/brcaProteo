#' a simple ggplot2 display for iTRAQ vs RPPA vs RNA-seq in brcaProteo::brint
#' @param gene character(1)
#' @param tx character(1) if 'none', raw data are displayed, otherwise z-scores (over tumors) are used
#' @return a ggplot instance with a horizontal grid, include info on pam50 class
#' @export
plot3 = function(gene="BCL2", tx="none") {
 zs = function(x) (x-mean(x,na.rm=TRUE))/sd(x, na.rm=TRUE)
 if (tx == "none") zs = force
 dd = data.frame(sapply(experiments(brint[gene,]), function(x)zs(assay(x))))
 rownames(dd) = colnames(brint)[[1]]
 cd = colData(experiments(brint)[[1]])
 dd$pam50 = cd$PAM50.mRNA
 ddd = rbind(data.frame(y=dd[,1],x=dd[,2],pam50=dd$pam50, id=rownames(dd), type="itraq vs RPPA"), 
   data.frame(y=dd[,1], x=dd[,3], pam50=dd$pam50, id=rownames(dd), type="itraq vs RNA-seq"), 
   data.frame(y=dd[,2],x=dd[,3], pam50=dd$pam50, id=rownames(dd), type="RPPA vs RNA-seq"))
 ddd$type = paste0(gene, ": ", ddd$type)
 ggplot(ddd, aes(x=x,y=y,colour=pam50, text=id)) + geom_point() + facet_grid(.~type) + ylab("var 1") + xlab("var 2")
}
