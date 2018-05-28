install.packages(c("choroplethr","choroplethrMaps"))
library(jsonlite)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(readr)
library(ggplot2)
data103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
data104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
data105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
data106<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")
colname<-c("洲別","國別","學位生_正式修讀學位外國生","學位生_僑生","學位生_正式修讀學位陸生","非學位生_外國交換生","非學位生_外國短期研習及個人選讀","非學位生_大專附設華語文中心學生","非學位生_大陸研修生","非學位生_海青班","境外專班")
colnames(data103)<-colname
colnames(data104)<-colname
colnames(data105)<-colname
colnames(data106)<-colname
data103to106<-rbind(data103,data104,data105,data106)
data103to106$學位生_正式修讀學位外國生<-as.numeric(data103to106$學位生_正式修讀學位外國生)
data103to106$學位生_僑生<-as.numeric(data103to106$學位生_僑生)
data103to106$學位生_正式修讀學位陸生<-as.numeric(data103to106$學位生_正式修讀學位陸生)
data103to106$非學位生_外國交換生<-as.numeric(data103to106$非學位生_外國交換生)
data103to106$非學位生_外國短期研習及個人選讀<-as.numeric(data103to106$非學位生_外國短期研習及個人選讀)
data103to106$非學位生_大專附設華語文中心學生<-as.numeric(data103to106$非學位生_大專附設華語文中心學生)
data103to106$非學位生_大陸研修生<-as.numeric(data103to106$非學位生_大陸研修生)
data103to106$非學位生_海青班<-as.numeric(data103to106$非學位生_海青班)
data103to106$境外專班<-as.numeric(data103to106$境外專班)

data103to106%>%
  group_by(國別)%>%
  mutate(總人數=學位生_正式修讀學位外國生+
                學位生_正式修讀學位陸生+
                學位生_僑生+
                非學位生_外國交換生+
                非學位生_外國短期研習及個人選讀+
                非學位生_大專附設華語文中心學生+
                非學位生_大陸研修生+
                非學位生_海青班+
                境外專班)%>%
  summarise(最終總人數=sum(總人數))%>%
  arrange(desc(最終總人數))%>%
  select(國別,最終總人數)%>%
  head(10)%>%
  View()

data103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
data104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
data105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
data106<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")
colname<-c("學校類型","學校代碼","學校名稱","學位生_正式修讀學位外國生","學位生_僑生","學位生_正式修讀學位陸生","非學位生_外國交換生","非學位生_外國短期研習及個人選讀","非學位生_大專附設華語文中心學生","非學位生_大陸研修生","非學位生_海青班","境外專班")
colnames(data103)<-colname
colnames(data104)<-colname
colnames(data105)<-colname
colnames(data106)<-colname
data103to106<-rbind(data103,data104,data105,data106)
data103to106$境外專班<-as.numeric(data103to106$境外專班)

data103to106%>%
  group_by(學校名稱)%>%
  summarise(境外生總人數=sum(境外專班))%>%
  arrange(desc(境外生總人數))%>%
  select(學校名稱,境外生總人數)%>%
  head(10)%>%
  View()



data103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
data104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
data105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
data106<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")
colname<-c("洲別","國別","學位生_正式修讀學位外國生","學位生_僑生","學位生_正式修讀學位陸生","非學位生_外國交換生","非學位生_外國短期研習及個人選讀","非學位生_大專附設華語文中心學生","非學位生_大陸研修生","非學位生_海青班","境外專班")
colnames(data103)<-colname
colnames(data104)<-colname
colnames(data105)<-colname
colnames(data106)<-colname
data103to106<-rbind(data103,data104,data105,data106)
data103to106$學位生_正式修讀學位外國生<-as.numeric(data103to106$學位生_正式修讀學位外國生)
data103to106$學位生_僑生<-as.numeric(data103to106$學位生_僑生)
data103to106$學位生_正式修讀學位陸生<-as.numeric(data103to106$學位生_正式修讀學位陸生)
data103to106$非學位生_外國交換生<-as.numeric(data103to106$非學位生_外國交換生)
data103to106$非學位生_外國短期研習及個人選讀<-as.numeric(data103to106$非學位生_外國短期研習及個人選讀)
data103to106$非學位生_大專附設華語文中心學生<-as.numeric(data103to106$非學位生_大專附設華語文中心學生)
data103to106$非學位生_大陸研修生<-as.numeric(data103to106$非學位生_大陸研修生)
data103to106$非學位生_海青班<-as.numeric(data103to106$非學位生_海青班)
data103to106$境外專班<-as.numeric(data103to106$境外專班)

dataToBar<-
  data103to106%>%
    group_by(洲別,國別)%>%
    mutate(總人數=學位生_正式修讀學位外國生+
              學位生_正式修讀學位陸生+
              學位生_僑生+
              非學位生_外國交換生+
              非學位生_外國短期研習及個人選讀+
              非學位生_大專附設華語文中心學生+
              非學位生_大陸研修生+
              非學位生_海青班+
              境外專班)%>%
    summarise(最終總人數=sum(總人數))%>%
    arrange(desc(最終總人數))

ggplot(dataToBar,aes(國別,最終總人數))+
  geom_bar(stat = "identity")+
  facet_grid(洲別~.)+
  labs(x="國家",y="來台留學生人數",title="自103年到106年各國來台學生")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dataToBar_list<-
  list(dataToBar_Asia<-dataToBar[dataToBar$洲別=="亞洲",],
    dataToBar_America<-dataToBar[dataToBar$洲別=="美洲",],
    dataToBar_Africa<-dataToBar[dataToBar$洲別=="非洲",],
    dataToBar_Europe<-dataToBar[dataToBar$洲別=="歐洲",],
    dataToBar_Oceania<-dataToBar[dataToBar$洲別=="大洋洲",])
Bartitle<-c("亞洲","美洲","非洲","歐洲","大洋洲")

for(i in 1:5){
  print(
      ggplot(dataToBar_list[[i]],aes(國別,最終總人數))+
        geom_bar(stat = "identity")+
        labs(x="國家",y="來台留學生人數",title=paste0(Bartitle[i],"  自103年到106年各國來台學生"))+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
  )
}

Comparision<-read_csv("GitHub/106bigdatacguimhw2-diandian1997-20180522T011729Z-001/106bigdatacguimhw2-diandian1997/CountriesComparisionTable.csv")
EnglihCountries<-c()
for(i in 1:177){
  for(j in 1:273){
    if(dataToBar[i,"國別"]==Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(dataToBar[i,"國別"]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")
  }
}
dataBar<-left_join(dataToBar,Comparision,by=c("國別"="Taiwan"))
dataToBar$Country<-array(unlist(EnglihCountries))
df = data.frame(region=dataBar$English, value=dataBar$最終總人數)
df<-df[!duplicated(df$region),]
foreginstudent_choropleth<-
  country_choropleth(df,"自103年到106年各國來台學生",num_colors=9)+
  scale_fill_brewer("總人數",palette=1)

TaiwanStudent<-
  read_csv("GitHub/106bigdatacguimhw2-diandian1997-20180522T011729Z-001/106bigdatacguimhw2-diandian1997/Student_RPT_07.csv")
#取出各個國家和總人數欄位
TaiwanStudent_clean<-
  TaiwanStudent%>%
  filter(學年度>=103)%>%
  group_by(對方學校.機構.國別.地區.)%>%
  summarise(總人數=sum(小計))%>%
  arrange(desc(總人數))
#將總人數欄位轉成數字
TaiwanStudent_clean$總人數<-as.numeric(TaiwanStudent_clean$總人數)
#因為發現資料中有很多想同國家但表示方式不同，所以以下gsub再將國家名稱一致化
TaiwanStudent_clean$對方學校.機構.國別.地區.<-
  gsub("共和國|和平之國|王國|侯國|聯邦|哈什米王國|阿拉伯共和國|泰王國|民主社會主義共和國|合眾國|大韓民國|大公國|社會主義|民主|伊斯蘭",
       "",
       TaiwanStudent_clean$對方學校.機構.國別.地區.)
#差異較大的名稱一一取出再做總人數的加總
TaiwanStudent_clean[TaiwanStudent_clean[,1]=='中國大陸',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='大陸地區',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='中國大陸',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='南韓',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='南韓',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='(南韓)',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='泰國',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='泰國',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='(泰國)',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='德國',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='德國',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='德意志',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='蒙古',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='蒙古',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='蒙古國',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='印尼',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='印尼',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='印度尼西亞',2]
#經過處理以後資料表中便有許多相同的國家，再將相同的國家總人數加起來
TaiwanStudent_clean<-
  TaiwanStudent_clean%>%
  group_by(對方學校.機構.國別.地區.)%>%
  summarise(總人數=sum(總人數))%>%
  arrange(desc(總人數))
#將剛剛差異較大的名稱的row移除
TaiwanStudent_clean<-TaiwanStudent_clean[
  !grepl("大陸地區|(南韓)|(泰國)|德意志|蒙古國|印度尼西亞",
         TaiwanStudent_clean$對方學校.機構.國別.地區.),]

TaiwanStudent_clean%>%
  arrange(desc(總人數))%>%
  head(10)%>%
  knitr::kable()

TaiwanStudent%>%
  group_by(學校名稱)%>%
  summarise(總人數=sum(小計))%>%
  arrange(desc(總人數))%>%
  head(10)

ggplot(TaiwanStudent_clean,aes(對方學校.機構.國別.地區.,總人數))+
  geom_bar(stat = "identity")+
  labs(x="國家",y="出國留學生人數",title="自103年到106年出國留學生")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dataToBAR<-
  rbind(head(TaiwanStudent_clean,33),
      slice(TaiwanStudent_clean,34:n()) %>% 
        summarise(對方學校.機構.國別.地區.="other",總人數=sum(總人數))
)
ggplot(dataToBAR,aes(對方學校.機構.國別.地區.,總人數))+
  geom_bar(stat = "identity")+
  labs(x="國家",y="出國留學生人數",title="自103年到106年出國留學生")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#這是R Code Chunk
EnglihCountries<-c()
for(i in 1:nrow(TaiwanStudent_clean)){
  for(j in 1:nrow(Comparision)){#將要畫表格的資料中代表國家欄位的資料取出再用對照表格建立對照英文國名
    if(TaiwanStudent_clean[i,"對方學校.機構.國別.地區."]==
       Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(TaiwanStudent_clean[i,"對方學校.機構.國別.地區."]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")#若沒有對照的英文則存成Unmatch
  }
}
#將剛做好的英文對照名稱加入表格
TaiwanStudent_clean$Country<-array(unlist(EnglihCountries))
#若要以choroplethr作面量圖的話必須是一個只有region和value欄位的dataframe
choropleth = data.frame(region=TaiwanStudent_clean$Country, value=TaiwanStudent_clean$總人數)
#因為用choroplethr做面量圖不能有重複的資料，刪除重複的資料(也就是剛剛Unmatch的資料)
choropleth<-choropleth[!duplicated(choropleth$region),]
#以country level來作面量圖
country_choropleth(choropleth)

Exchangestudent<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
Exchangestudent[,4:6]<-NULL
Exchangestudent$總人數<-as.numeric(Exchangestudent$總人數)
Exchangestudent%>%
  arrange(desc(總人數))%>%
  select('國別','總人數')%>%
  head(10)

EnglihCountries<-c()
for(i in 1:29){
  for(j in 1:273){#將要畫表格的資料中代表國家欄位的資料取出再用對照表格建立對照英文國名
    if(Exchangestudent[i,"國別"]==CountriesComparisionTable[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,CountriesComparisionTable[j,"English"])
      break
    }else if(Exchangestudent[i,"國別"]!=CountriesComparisionTable[j,"Taiwan"]&j==272)
      EnglihCountries<-c(EnglihCountries,"Unmatch")#若沒有對照的英文則存成Unmatch
  }
}
#將剛做好的英文對照名稱加入表格
Exchangestudent$Country<-array(unlist(EnglihCountries))
#若要以choroplethr作面量圖的話必須是一個只有region和value欄位的dataframe
choropleth = data.frame(region=Exchangestudent$Country, value=Exchangestudent$總人數)
#因為用choroplethr做面量圖不能有重複的資料，刪除重複的資料(也就是剛剛Unmatch的資料)
choropleth<-choropleth[!duplicated(choropleth$region),]
#以country level來作面量圖
country_choropleth(choropleth)

con<-file('CountriesComparisionTable.csv',encoding="UTF-8")
write.csv(CountriesComparisionTable,file=con,row.names = F)


EnglihCountries<-c()
for(i in 1:177){
  for(j in 1:273){
    if(dataToBar[i,"國別"]==Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(dataToBar[i,"國別"]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")
  }
}
dataToBar$Country<-array(unlist(EnglihCountries))
df = data.frame(region=dataToBar$Country, value=dataToBar$最終總人數)
df<-df[!duplicated(df$region),]
country_choropleth(df)+
  scale_fill_brewer(palette=1) 

EnglihCountries<-c()
for(i in 1:nrow(TaiwanStudent_clean)){
  for(j in 1:nrow(Comparision)){#將要畫表格的資料中代表國家欄位的資料取出再用對照表格建立對照英文國名
    if(TaiwanStudent_clean[i,"對方學校.機構.國別.地區."]==
       Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(TaiwanStudent_clean[i,"對方學校.機構.國別.地區."]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")#若沒有對照的英文則存成Unmatch
  }
}
#將剛做好的英文對照名稱加入表格
TaiwanStudent_clean$Country<-array(unlist(EnglihCountries))
#若要以choroplethr作面量圖的話必須是一個只有region和value欄位的dataframe
choropleth = data.frame(region=TaiwanStudent_clean$Country, value=TaiwanStudent_clean$總人數)
#因為用choroplethr做面量圖不能有重複的資料，刪除重複的資料(也就是剛剛Unmatch的資料)
choropleth<-choropleth[!duplicated(choropleth$region),]
#以country level來作面量圖
country_choropleth(choropleth)+
  scale_fill_brewer(palette=8) 
