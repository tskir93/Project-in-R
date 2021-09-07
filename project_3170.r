getwd()

raw.data<- read.table("GDS4879.clean",sep="\t", header=TRUE)
data <- raw.data[,-c(1,2)]
namesindex <- as.character(raw.data[,2]);
colNum = dim(data)[2];

#boxplot without log2
pdf("boxplot.pdf")
boxplot(data)
dev.off()

#correct boxplot with log2
data2 <- log2(data)
pdf("boxplot2.pdf")
boxplot(data2)
dev.off()

data

#question 3
labels <- c(rep("alcoholism",6),rep("control",6),rep("alcoholism",14),rep("control",13))
labels

dim(data)
unique(labels)

myttest <- function(v,labels){
  levels <- unique(labels)
  v1 <- v[labels == levels[1]]
  v2 <- v[labels == levels[2]]
  pval <- t.test(v1, v2, alternative = c("greater"), var.equal = F)$p.value 
  pval
}

allpvalues1 <- apply(data2, 1, myttest, labels)
allpvalues3 <- apply(data2, 1, myttest, labels)
#pvalues1 <- apply(data, 1, myttest, 1:6, 7:12)
pval1<-allpvalues1[allpvalues1 < 0.05]
pval1
#length(allpvalues)
length(pval1)


#question 4
labels2 <- c(rep("female",12),rep("male",27))
labels2

unique(labels2)

myttest <- function(v, labels2){
  levels2 <- unique(labels2)
  v3 <- v[labels2 == levels2[1]]  #gunaikes
  v4 <- v[labels2 == levels2[2]]  #andres
  pval2 <- t.test(v3, v4, alternative = c("greater"), var.equal = F)$p.value 
  pval2
}

allpvalues2 <- apply(data2, 1, myttest, labels2)
allpvalues4 <- apply(data2, 1, myttest, labels2)
#pvalues1 <- apply(data, 1, myttest, 1:6, 7:12)
pval3<-allpvalues2[allpvalues2 < 0.05]
pval3
#length(allpvalues)
length(pval3)


#question 5
#FDR correction for q3
allpvalues3<-p.adjust(allpvalues3, "fdr")
#get the < 0.05 pvalues
pvalfdr<-allpvalues3[allpvalues3 < 0.05]

#Print number of genes after FDR correction
length(pvalfdr)


#Bonferroni correction for q3
allpvalues1<-p.adjust(allpvalues1, "bonferroni")
#get the < 0.05 pvalues
pvalbon<-allpvalues1[allpvalues1 < 0.05]

length(pvalbon)

#FDR correction for q4
allpvalues2<-p.adjust(allpvalues2, "fdr")
#get the < 0.05 pvalues
pvalfdr1<-allpvalues2[allpvalues2 < 0.05]

#Print number of genes after FDR correction
length(pvalfdr1)

#Bonferroni correction for q4
allpvalues4<-p.adjust(allpvalues4, "bonferroni")
#get the < 0.05 pvalues
pvalbon1<-allpvalues4[allpvalues4 < 0.05]

length(pvalbon1)


#question 7

mat <- matrix(c(rnorm(2500, 100, 10), rnorm(2500, 180,10) ), nrow = 100, byrow=F)

head(mat)
mypca<-prcomp(t(mat), center = TRUE, scale. = TRUE)

names(mypca)

mypca
summary(mypca)
colvec = c(rep("red", 12), rep("blue", 27))
# c(rep("alcoholism",6),rep("control",6),rep("alcoholism",14),rep("control",13))
symbol = rep( c(rep(3, 6),rep(19, 6),rep(3,14),rep(19,13), 2))
colvec
pdf("pca.pdf")
plot(mypca$x[,1], mypca$x[,2], col=colvec, pch=symbol)
dev.off()


