summarize.HierarchicalBoosting <-
function(results){

cat("Summarizing classification...\n")

classification<-data.frame(chromosome=results$chromosome,start=results$start,end=results$end,stringsAsFactors=FALSE)
res_class<-results[,grep("classification",colnames(results))]
class<-c()
for (n in 1:length(res_class[,1])){
for (i in length(res_class):1){
if(is.na(res_class[n,i])){
if(i==1){class<-c(class,res_class[n,i])}
}else{
class<-c(class,res_class[n,i])
break
}
}
}
classification$classification<-class
return(classification)
}
