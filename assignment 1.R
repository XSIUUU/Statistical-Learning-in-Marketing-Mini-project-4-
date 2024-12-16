group9 <- SLIM2425a_team_9_glm


library(gt)
install.packages("openxlsx")
library(openxlsx)

###################################
##########Life style###############
###################################

ls <- group9[,11:23]
ls.sd <- data.frame(scale(ls))
summary(ls.sd)



library(nFactors)
nScree(ls.sd)     #recommended factor num 1-2
eigen(cor(ls.sd))  #regards to eigenvalue, 5 factors recommended



#####

kmo <- function(x){
  x <- subset(x, complete.cases(x))
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d, d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

kmo(ls.sd)  #KMO=0.68, proportion of variability among variables that might be common variability

#####

bartlett.sphere<-function(data){chi.square=-( (dim(data)[1]-1) -
                                                (2*dim(data)[2]-5)/6 )*
  log(det(cor(data,use='pairwise.complete.obs')));cat('chi.square value ',
                                                      chi.square , ' on ', (dim(data)[2]^2-dim(data)[2])/2, ' degrees of freedom.'
                                                      , ' p-value: ', 1-pchisq(chi.square,(dim(data)[2]^2-dim(data)[2])/2))}

bartlett.sphere(ls.sd)   #p-value:  0, H0 rejected, common variance among variables

#####

library(GPArotation)
(ls.sd.ob <- factanal(ls.sd, factors=3, rotation="oblimin",scores = "Bartlett"))   
ls.scores <- data.frame(ls.sd.ob$scores)
group9$ls_scores <- ls.scores

#####

library(gplots)
library(RColorBrewer)
heatmap.2(ls.sd.ob$loadings, 
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for brand adjectives")


#####

library(semPlot)
semPaths(ls.sd.ob, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)
###



#####################################
##### perception of bio food#########
#####################################

pc <- group9[,24:34]
pc.sd <- data.frame(scale(pc))
summary(pc.sd)

library(nFactors)
nScree(pc.sd)     #recommended factor num 1-2
eigen(cor(pc.sd))  #regards to eigenvalue, 5 factors recommended


####


library(corrplot)
corrplot(cor(pc.sd), order="hclust")

plot(prcomp(pc.sd),type="l")



#####

kmo <- function(x){
  x <- subset(x, complete.cases(x))
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d, d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

kmo(pc.sd)  #KMO=0.73, proportion of variability among variables that might be common variability

#####

bartlett.sphere<-function(data){chi.square=-( (dim(data)[1]-1) -
                                                (2*dim(data)[2]-5)/6 )*
  log(det(cor(data,use='pairwise.complete.obs')));cat('chi.square value ',
                                                      chi.square , ' on ', (dim(data)[2]^2-dim(data)[2])/2, ' degrees of freedom.'
                                                      , ' p-value: ', 1-pchisq(chi.square,(dim(data)[2]^2-dim(data)[2])/2))}

bartlett.sphere(pc.sd)   #p-value:  0, H0 rejected, common variance among variables

#####

library(GPArotation)
pc.sd.ob <- factanal(pc.sd, factors=3, rotation="oblimin",scores="Bartlett")   #p-value of chi square is too small, recommend more factors
pc.scores <- data.frame(pc.sd.ob$scores)
group9$pc_scores <- pc.scores


#####

library(gplots)
library(RColorBrewer)
heatmap.2(pc.sd.ob$loadings, 
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for brand adjectives")


#####

library(semPlot)
semPaths(pc.sd.ob, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)


