
library(overlapping)

# sentences
original = read.csv("/data/features_values_distributions_sentences_pk_env.csv")
totake = c(3,4,5,6,7,10,14)
idx = names(original)[totake]
ip = read.csv("/data/features_values_distributions_sentences_ip_pk_env.csv")

#get overlapping coeff for each variables and each subject
vide <- matrix( , nrow = 21, ncol = 7)
for (i in 1:21){
  subi = subset(ip,ip$suj ==i)
  for (j in 1:7){
    varname = idx[j]
    su = subi[varname]
    dataList <- list(normr = as.numeric(unlist(original[varname])), ips= as.numeric(unlist(su)))
    ovi = overlap( dataList )$OV*100
    vide[i,j] <- ovi
  }
}
overlaps <- as.data.frame(vide)
names(overlaps) <-idx
write.csv(overlaps,"/data/overlap_sentences_pk_env.csv", row.names = TRUE)



# words
original = read.csv("/data/selected_distribution_monosyllabic_words_pk_env.csv")
idx = names(original)[c(2,3,4,5,6,7)]
ip = read.csv("/data/features_values_distributions_words_ip_pk_env.csv")

#get overlapping coeff for each variables and each subject
vide <- matrix( , nrow = 21, ncol = 6)
for (i in 1:21){
  subi = subset(ip,ip$suj ==i)
  for (j in 1:6){
    varname = idx[j]
    su = subi[varname]
    dataList <- list(normr = as.numeric(unlist(original[varname])), ips= na.omit(as.numeric(unlist(su))))
    ovi = overlap( dataList )$OV*100
    vide[i,j] <- ovi
  }
}
overlaps <- as.data.frame(vide)
names(overlaps) <-idx
write.csv(overlaps,"/data/overlap_words_pk_env.csv", row.names = TRUE)

