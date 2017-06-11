library(ggplot2)
require("DT")
library(networkD3)

shinyServer(
  function(input, output) {

#component2 数据导入
 output$contents <- renderTable({
  inFile <- input$file1
  
   if (is.null(inFile))
      return(NULL)
data<-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote,row.names=1)   				
				data[1:6,1:10]
})


output$distPlot <- renderPlot({
inFile <- input$file1
if (is.null(inFile)) return(NULL)
data<-read.csv(inFile$datapath, header=TRUE,quote="\t",row.names=1)
missing<-data.frame("样品名称"=c(NA),"有效标记百分比"=c(NA),"有效标记数目"=c(NA),"总标记数"=c(NA),"顺序"=c(NA))
for (i in 1:nrow(data)){
n<-length(data[i,which(data[i,]=="?" | data[i,]=="NA" )])
missing[i,1]<-row.names(data)[i]
missing[i,2]<-(1-n/ncol(data))*100
missing[i,3]<-ncol(data)-n
missing[i,4]<-ncol(data)
}
missing<-missing[order(missing[,2],decreasing=TRUE),]
missing[,5]<-c(1:nrow(missing))


output$downloadData1 <- downloadHandler(
    filename = function() {
       paste("Sampleinfo-", Sys.time(), ".csv", sep="")
     },
    content = function(file) {
      write.csv(missing, file, row.names=FALSE)
    })
	
		
output$table1 <- DT::renderDataTable(DT::datatable({
	missing[,1:5]
	}))
	
	
g <- ggplot(missing, aes(missing[,5],missing[,2]))
g + geom_line(colour="tomato",size=3,pch=16)+
scale_x_continuous(limits=c(0,nrow(missing)),breaks=seq(1,nrow(missing),round(nrow(missing)/20)),labels=missing[seq(1,nrow(missing),round(nrow(missing)/20)),1])+
theme(panel.background = element_rect(fill = "white"))+
labs(x="材料",y="有效标记百分比（%100）")+
theme(axis.line = element_line(size = 1.2, colour = "black"),axis.ticks = element_line(size = 1.2),
axis.ticks.length = unit(.15, "cm"),axis.text = element_text(size= 12),axis.title= element_text(size = rel(1.5))
)
  })
  
  
#component3
   
  #################################################

output$text1 <- renderText({ 
      paste("与材料", input$samplename,"的相似性结果")
    })
	
output$text2 <- renderText({ 
      paste("相似性分析")
    }) 
	
output$text3 <- renderText({ 
      paste("注意区分大小写及不要有空格,点击“Submit”按键或按Enter 键")
    }) 
	
output$text4 <- renderText({ 
      paste("下载与材料", input$samplename,"的相似性分析结果表")
    }) 

output$text5 <- renderText({ 
      paste("相似性分析结果")
    }) 	
	
output$text6 <- renderText({ 
      paste("标记信息统计表")
    })

output$text7 <- renderText({ 
      paste("  材料间相似性网络")
    })
 	
  
output$distPlot2 <- renderPlot({
 
 inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
    
  data<-as.matrix(read.csv(inFile$datapath, header=TRUE, 
,quote="\t",row.names=1,stringsAsFactors = FALSE)) 
standard<-as.vector(data[which(row.names(data)==input$samplename),])

similarity<-data.frame(Sample1=c(NA),Sample2=c(NA),"Similarity*%"=c(NA),MarkerUsed=c(NA))
for(i in 1:nrow(data)){
k<-0
n<-0
for(j in 1:length(standard)){
if(length(strsplit(standard[j],split=":")[[1]])==2 & length(strsplit(as.vector(data[i,][j]),split=":")[[1]])==2){
s1<-strsplit(standard[j],split=":")[[1]][1]
s2<-strsplit(standard[j],split=":")[[1]][2]
a1<-strsplit(as.vector(data[i,][j]),split=":")[[1]][1]
a2<-strsplit(as.vector(data[i,][j]),split=":")[[1]][2]
if(s1==a1){
k<-k+0.5}
if(s2==a2){
k<-k+0.5}
n<-n+1}
}
similarity[i,1]<-input$samplename
similarity[i,2]<-row.names(data)[i]
similarity[i,3]<-round(k/n*100,2)
similarity[i,4]<-n
}



	
output$downloadData <- downloadHandler(

    filename = function() {
       paste("Resultinfo-",input$samplename,"-", Sys.time(), ".csv", sep="")
     },
    content = function(file) {
      write.csv(similarity, file)
    })	
	
	
output$table2 <- DT::renderDataTable(DT::datatable({
  similarity
	}))

sim<-similarity[order(similarity[,3],decreasing=TRUE),]
sim[,5]<-c(1:nrow(sim))
g <- ggplot(similarity, aes(sim[,5],sim[,3]))
g + geom_col(fill="tomato",size=1,pch=2,color="tomato")+
scale_x_continuous(limits=c(0,(nrow(sim)+1)),breaks=seq(1,nrow(sim),round(nrow(sim)/20)),labels=sim[seq(1,nrow(sim),round(nrow(sim)/20)),2])+
theme(panel.background = element_rect(fill = "white"))+
labs(x="材料",y="相似性（%100）")+
theme(axis.line = element_line(size = 1.2, colour = "black"),axis.ticks = element_line(size = 1.2),
axis.ticks.length = unit(.15, "cm"),axis.text = element_text(size= 12),axis.title= element_text(size = rel(1.5))
)
})

library(networkD3)
output$force <- renderForceNetwork({

#setwd("C:/Users/kun/Desktop/（无主题）")
#data1<-read.table("Genotyping-2808.200-01 Grid.csv",header=TRUE,row.names=1,sep=",",stringsAsFactors=FALSE)
inFile <- input$file2
if (is.null(inFile)) return(NULL)
data1<-read.table(inFile$datapath,header=TRUE,row.names=1,sep=",",stringsAsFactors=FALSE)
data<-data1
class<-data.frame(Class=c(NA),Typical=c(NA),Size=c(NA),SpecificSsample=c(NA))
g<-1
for(i in 1:nrow(data)){
n=0
similarity<-data.frame(Sample1=c(NA),Sample2=c(NA),"Similarity*%"=c(NA))
if(nrow(data)>=1){
standard<-as.vector(as.matrix(data[1,]))
for(k in 1:nrow(data)){
m=0
l=0
for(j in 1:length(standard)){

if(length(strsplit(standard[j],split=":")[[1]])==2 & length(strsplit(data[k,j],split=":")[[1]])==2){
s1<-strsplit(standard[j],split=":")[[1]][1]
s2<-strsplit(standard[j],split=":")[[1]][2]
a1<-strsplit(data[k,j],split=":")[[1]][1]
a2<-strsplit(data[k,j],split=":")[[1]][2]
if(s1==a1){m<-m+0.5}
if(s2==a2){m<-m+0.5}
l<-l+1}
}
n<-n+1
similarity[n,1]<-row.names(data)[1]
similarity[n,2]<-row.names(data)[k]
similarity[n,3]<-i-1
similarity[n,4]<-k-1
similarity[n,5]<-round(m/l*100,2)
}
cla=""
similarity<-similarity[which(similarity[,5]==100),]
for(cl in 1:nrow(similarity)){cla<-paste(cla,similarity[cl,2],sep=",")}
class[g,1]<-paste("class",g,sep="")
class[g,2]<-similarity[1,1]
class[g,3]<-nrow(similarity)
class[g,4]<-cla
g<-g+1
for(f in 1:nrow(similarity)){data<-data[-which(row.names(data)==similarity[f,2]),]}
}
}

similarity<-data.frame(Sample1=c(NA),Sample2=c(NA),"Similarity*%"=c(NA),MarkerUsed=c(NA))
i=1
for(a in 1:(nrow(class)-1)){
standard<-as.matrix(data1[which(row.names(data1)==class[a,2]),])
for(b in (a+1):nrow(class)){
data<-as.matrix(data1[which(row.names(data1)==class[b,2]),])
k<-0
n<-0
for(j in 1:length(standard)){
if(length(strsplit(standard[j],split=":")[[1]])==2 & length(strsplit(data[j],split=":")[[1]])==2){
s1<-strsplit(standard[j],split=":")[[1]][1]
s2<-strsplit(standard[j],split=":")[[1]][2]
a1<-strsplit(data[j],split=":")[[1]][1]
a2<-strsplit(data[j],split=":")[[1]][2]
if(s1==a1){
k<-k+0.5}
if(s2==a2){
k<-k+0.5}
n<-n+1}
}
similarity[i,1]<-class[a,2]
similarity[i,2]<-class[b,2]
similarity[i,3]<-a-1
similarity[i,4]<-b-1
similarity[i,5]<-round(k/n*100,2)
i=i+1
}
}


similarity<-similarity[which(similarity[,5]>=75),]
similarity1<-similarity[,-1:-2]
names(similarity1)<-c("source","target","value")
similarity2<-data.frame("name"=c(NA),"group"=c(NA),"size"=c(NA))
s<-unique(c(similarity[,1],similarity[,2]))
s1<-s
s2<-s
for(i in 1:length(s)){s1[i]<-class[which(class[,2]==s[i]),][1,1]}
for(i in 1:length(s)){s2[i]<-class[which(class[,2]==s[i]),][1,3]}
similarity2[1:length(s),1]<-s1
similarity2[1:length(s),2]<-c(1:length(s))
similarity2[1:length(s),3]<-s2


output$class <- renderPlot({
g <- ggplot(class, aes(class[,1],class[,3]))
g + geom_col(fill="tomato",size=1,pch=2,color="tomato")+
geom_text(aes(class[,1],(class[,3]+max(class[,3])*0.1),label=class[,3]))+
theme(panel.background = element_rect(fill = "white"))+
labs(x="类别",y="材料个数")+
theme(axis.line = element_line(size = 1.2, colour = "black"),axis.ticks = element_line(size = 1.2),
axis.ticks.length = unit(.15, "cm"),axis.text = element_text(size= 12),axis.title= element_text(size = rel(1.5)))
})

output$classinfo <- downloadHandler(

    filename = function() {
       paste("Classificationinfo-", Sys.time(), ".csv", sep="")
     },
    content = function(file) {
      write.csv(class, file)
    })	

forceNetwork(Links = similarity1, Nodes = similarity2,
            Source = "source", Target = "target",
			Nodesize = 'size',NodeID = "name",radiusCalculation = JS("Math.pow(Math.sqrt(d.nodesize),2)"),
			#radiusCalculation = "d.nodesize",
            Group = "group", opacity = 1, legend = F, bounded = T,charge=-100,
			fontSize=20,fontFamily = "serif", opacityNoHover =1 ,zoom = T,#linkDistance = 100
			height=1500,width=600)
})

  
  })





