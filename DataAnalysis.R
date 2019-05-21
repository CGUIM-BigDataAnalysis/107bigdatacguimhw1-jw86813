library(dplyr)
library(readr)
X103 <- read_csv("103年各教育程度別初任人員經常性薪資─按大職類分.csv")
X104 <- read_csv("104年各教育程度別初任人員經常性薪資─按大職類分.csv")
X105 <- read_csv("105年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")
X106 <- read_csv("106年各教育程度別初任人員每人每月經常性薪資─按大職類分.csv")

names(X106)[names(X106)=="大學-薪資"]="106大學-薪資"
compare1<-inner_join(X103,X106,"大職業別") 

compare1$`106大學-薪資`<- as.numeric(compare1$`106大學-薪資`)
compare1$`大學-薪資`<- as.numeric(compare1$`大學-薪資`)

compare1<-mutate(compare1,薪資差異=(compare1$`106大學-薪資`/compare1$`大學-薪資`))%>%
 filter(薪資差異>1)
compare1$大職業別
result<-filter(compare1,薪資差異>=1.05)%>%
  select("大職業別",薪資差異)%>%
  arrange(desc(薪資差異))
head(result,10)

result$大職業別<-substr(result$大職業別, 
                    start=1,stop=regexpr("業",result$大職業別))
sort(table(result$大職業別),decreasing = T)

salaryvs<-select(X103,"大職業別")%>%
  mutate(vs103=X103$`專科-女/男`,vs104=X104$`大學-女/男`,
         vs105=X105$`大學-女/男`,vs106=X106$`大學-女/男`,)
salaryvs$vs103<-as.numeric(salaryvs$vs103)
salaryvs$vs104<-as.numeric(salaryvs$vs104)
salaryvs$vs105<-as.numeric(salaryvs$vs105)
salaryvs$vs106<-as.numeric(salaryvs$vs106)
salaryvs$四年平均<-rowMeans(salaryvs[,2:5],na.rm = T) 
manwin<-arrange(salaryvs,四年平均)
head(manwin,10)
womanwin<-arrange(salaryvs,desc(四年平均))
head(womanwin,10)

X106$`106大學-薪資`<-as.numeric(X106$`106大學-薪資`)
X106$`研究所及以上-薪資`<-as.numeric(X106$`研究所及以上-薪資`)
X106<-mutate(X106,薪資比例=`研究所及以上-薪資`/`106大學-薪資` )%>%
  select("大職業別","106大學-薪資","研究所及以上-薪資",薪資比例)%>%
  arrange(desc(薪資比例))
head(X106,10)

myfav<-filter(X106,大職業別%in% c("資訊及通訊傳播業",
                                "藝術_娛樂及休閒服務業-專業人員",
                                "工業部門-專業人員","營造業",
                                "運輸及倉儲業") )
myfav<-mutate(myfav,diff=(`研究所及以上-薪資`- `106大學-薪資` ))
myfav

