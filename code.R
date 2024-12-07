##PCA
trait<-read.table("plant_trait.txt" , 
                  row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, header=T)
Group<-read.table("group.txt" , 
                  row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, header=T)

pca_trait <- rda(trait, scale = TRUE)
summary(pca_trait, scaling = 1)
summary(pca_trait, scaling = 2)

trait.scaling2 <- scores(pca_trait, choices = 1:2, scaling = 2, display = 'sp')
pca_eig <- pca_trait$CA$eig
pca_exp <- pca_trait$CA$eig / sum(pca_trait$CA$eig)
pc1 <- paste('PC1:', round(pca_exp[1]*100, 2), '%')
pc2 <- paste('PC2:',round(pca_exp[2]*100, 2), '%')
par(mfrow = c(1, 2))
cleanplot.pca(pca_trait)

#Stepwise regression


env <- read.delim('climate factor.txt', sep = '\t', row.names = 1)
spe  <- read.delim('r-k ratio.txt', sep = '\t', row.names = 1)

library(relaimpo)
library(MASS)


spe<-data.frame(spe)
spe_name <- 'Saprotroph.ECM'
env_spe <- cbind(env, spe[spe_name])
colnames(env_spe)[ncol(env_spe)] <- 'spe'
env_spe$spe<-spe$Saprotroph.ECM
env_spe<-data.frame(env_spe)
head(env_spe)

fit <- lm(spe~., data = env_spe)


fit_simple <- stepAIC(fit, direction = 'backward')
lm_stat <- summary(fit_simple)


lm_stat$r.squared  
lm_stat$adj.r.squared 
F <- lm_stat$fstatistic
pf(F[1], F[2], F[3], lower.tail = FALSE)  

crf <- calc.relimp(fit_simple, rela = FALSE)
crf$lmg
