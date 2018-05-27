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
colname<-c("�w�O","��O","�Ǧ��_������Ū�Ǧ�~���","�Ǧ��_����","�Ǧ��_������Ū�Ǧ쳰��","�D�Ǧ��_�~��洫��","�D�Ǧ��_�~��u����ߤέӤH��Ū","�D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�","�D�Ǧ��_�j����ץ�","�D�Ǧ��_���C�Z","�ҥ~�M�Z")
colnames(data103)<-colname
colnames(data104)<-colname
colnames(data105)<-colname
colnames(data106)<-colname
data103to106<-rbind(data103,data104,data105,data106)
data103to106$�Ǧ��_������Ū�Ǧ�~���<-as.numeric(data103to106$�Ǧ��_������Ū�Ǧ�~���)
data103to106$�Ǧ��_����<-as.numeric(data103to106$�Ǧ��_����)
data103to106$�Ǧ��_������Ū�Ǧ쳰��<-as.numeric(data103to106$�Ǧ��_������Ū�Ǧ쳰��)
data103to106$�D�Ǧ��_�~��洫��<-as.numeric(data103to106$�D�Ǧ��_�~��洫��)
data103to106$�D�Ǧ��_�~��u����ߤέӤH��Ū<-as.numeric(data103to106$�D�Ǧ��_�~��u����ߤέӤH��Ū)
data103to106$�D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�<-as.numeric(data103to106$�D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�)
data103to106$�D�Ǧ��_�j����ץ�<-as.numeric(data103to106$�D�Ǧ��_�j����ץ�)
data103to106$�D�Ǧ��_���C�Z<-as.numeric(data103to106$�D�Ǧ��_���C�Z)
data103to106$�ҥ~�M�Z<-as.numeric(data103to106$�ҥ~�M�Z)

data103to106%>%
  group_by(��O)%>%
  mutate(�`�H��=�Ǧ��_������Ū�Ǧ�~���+
                �Ǧ��_������Ū�Ǧ쳰��+
                �Ǧ��_����+
                �D�Ǧ��_�~��洫��+
                �D�Ǧ��_�~��u����ߤέӤH��Ū+
                �D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�+
                �D�Ǧ��_�j����ץ�+
                �D�Ǧ��_���C�Z+
                �ҥ~�M�Z)%>%
  summarise(�̲��`�H��=sum(�`�H��))%>%
  arrange(desc(�̲��`�H��))%>%
  select(��O,�̲��`�H��)%>%
  head(10)%>%
  View()

data103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
data104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
data105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
data106<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")
colname<-c("�Ǯ�����","�ǮեN�X","�ǮզW��","�Ǧ��_������Ū�Ǧ�~���","�Ǧ��_����","�Ǧ��_������Ū�Ǧ쳰��","�D�Ǧ��_�~��洫��","�D�Ǧ��_�~��u����ߤέӤH��Ū","�D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�","�D�Ǧ��_�j����ץ�","�D�Ǧ��_���C�Z","�ҥ~�M�Z")
colnames(data103)<-colname
colnames(data104)<-colname
colnames(data105)<-colname
colnames(data106)<-colname
data103to106<-rbind(data103,data104,data105,data106)
data103to106$�ҥ~�M�Z<-as.numeric(data103to106$�ҥ~�M�Z)

data103to106%>%
  group_by(�ǮզW��)%>%
  summarise(�ҥ~���`�H��=sum(�ҥ~�M�Z))%>%
  arrange(desc(�ҥ~���`�H��))%>%
  select(�ǮզW��,�ҥ~���`�H��)%>%
  head(10)%>%
  View()



data103<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
data104<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
data105<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
data106<-fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")
colname<-c("�w�O","��O","�Ǧ��_������Ū�Ǧ�~���","�Ǧ��_����","�Ǧ��_������Ū�Ǧ쳰��","�D�Ǧ��_�~��洫��","�D�Ǧ��_�~��u����ߤέӤH��Ū","�D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�","�D�Ǧ��_�j����ץ�","�D�Ǧ��_���C�Z","�ҥ~�M�Z")
colnames(data103)<-colname
colnames(data104)<-colname
colnames(data105)<-colname
colnames(data106)<-colname
data103to106<-rbind(data103,data104,data105,data106)
data103to106$�Ǧ��_������Ū�Ǧ�~���<-as.numeric(data103to106$�Ǧ��_������Ū�Ǧ�~���)
data103to106$�Ǧ��_����<-as.numeric(data103to106$�Ǧ��_����)
data103to106$�Ǧ��_������Ū�Ǧ쳰��<-as.numeric(data103to106$�Ǧ��_������Ū�Ǧ쳰��)
data103to106$�D�Ǧ��_�~��洫��<-as.numeric(data103to106$�D�Ǧ��_�~��洫��)
data103to106$�D�Ǧ��_�~��u����ߤέӤH��Ū<-as.numeric(data103to106$�D�Ǧ��_�~��u����ߤέӤH��Ū)
data103to106$�D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�<-as.numeric(data103to106$�D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�)
data103to106$�D�Ǧ��_�j����ץ�<-as.numeric(data103to106$�D�Ǧ��_�j����ץ�)
data103to106$�D�Ǧ��_���C�Z<-as.numeric(data103to106$�D�Ǧ��_���C�Z)
data103to106$�ҥ~�M�Z<-as.numeric(data103to106$�ҥ~�M�Z)

dataToBar<-
  data103to106%>%
    group_by(�w�O,��O)%>%
    mutate(�`�H��=�Ǧ��_������Ū�Ǧ�~���+
              �Ǧ��_������Ū�Ǧ쳰��+
              �Ǧ��_����+
              �D�Ǧ��_�~��洫��+
              �D�Ǧ��_�~��u����ߤέӤH��Ū+
              �D�Ǧ��_�j�M���]�ػy�夤�߾ǥ�+
              �D�Ǧ��_�j����ץ�+
              �D�Ǧ��_���C�Z+
              �ҥ~�M�Z)%>%
    summarise(�̲��`�H��=sum(�`�H��))%>%
    arrange(desc(�̲��`�H��))

ggplot(dataToBar,aes(��O,�̲��`�H��))+
  geom_bar(stat = "identity")+
  facet_grid(�w�O~.)+
  labs(x="��a",y="�ӥx�d�ǥͤH��",title="��103�~��106�~�U��ӥx�ǥ�")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dataToBar_list<-
  list(dataToBar_Asia<-dataToBar[dataToBar$�w�O=="�Ȭw",],
    dataToBar_America<-dataToBar[dataToBar$�w�O=="���w",],
    dataToBar_Africa<-dataToBar[dataToBar$�w�O=="�D�w",],
    dataToBar_Europe<-dataToBar[dataToBar$�w�O=="�ڬw",],
    dataToBar_Oceania<-dataToBar[dataToBar$�w�O=="�j�v�w",])
Bartitle<-c("�Ȭw","���w","�D�w","�ڬw","�j�v�w")

for(i in 1:5){
  print(
      ggplot(dataToBar_list[[i]],aes(��O,�̲��`�H��))+
        geom_bar(stat = "identity")+
        labs(x="��a",y="�ӥx�d�ǥͤH��",title=paste0(Bartitle[i],"  ��103�~��106�~�U��ӥx�ǥ�"))+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
  )
}

Comparision<-read_csv("GitHub/106bigdatacguimhw2-diandian1997-20180522T011729Z-001/106bigdatacguimhw2-diandian1997/CountriesComparisionTable.csv")
EnglihCountries<-c()
for(i in 1:177){
  for(j in 1:273){
    if(dataToBar[i,"��O"]==Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(dataToBar[i,"��O"]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")
  }
}
dataToBar$Country<-array(unlist(EnglihCountries))
df = data.frame(region=dataToBar$Country, value=dataToBar$�̲��`�H��)
df<-df[!duplicated(df$region),]
foreginstudent_choropleth<-
  country_choropleth(df,"��103�~��106�~�U��ӥx�ǥ�")+
    scale_fill_brewer("�`�H��",palette=1)

TaiwanStudent<-
  read_csv("GitHub/106bigdatacguimhw2-diandian1997-20180522T011729Z-001/106bigdatacguimhw2-diandian1997/Student_RPT_07.csv")
#���X�U�Ӱ�a�M�`�H�����
TaiwanStudent_clean<-
  TaiwanStudent%>%
  group_by(���Ǯ�.���c.��O.�a��.)%>%
  summarise(�`�H��=sum(�p�p))%>%
  arrange(desc(�`�H��))
#�N�`�H������ন�Ʀr
TaiwanStudent_clean$�`�H��<-as.numeric(TaiwanStudent_clean$�`�H��)
#�]���o�{��Ƥ����ܦh�Q�P��a�����ܤ覡���P�A�ҥH�H�Ugsub�A�N��a�W�٤@�P��
TaiwanStudent_clean$���Ǯ�.���c.��O.�a��.<-
  gsub("�@�M��|�M������|����|�J��|�p��|�����̤���|���ԧB�@�M��|������|���D���|�D�q�@�M��|�X����|�j������|�j����|���|�D�q|���D|�촵��",
       "",
       TaiwanStudent_clean$���Ǯ�.���c.��O.�a��.)
#�t�����j���W�٤@�@���X�A���`�H�ƪ��[�`
TaiwanStudent_clean[TaiwanStudent_clean[,1]=='����j��',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�j���a��',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='����j��',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�n��',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�n��',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='(�n��)',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='����',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='����',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='(����)',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�w��',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�w��',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�w�N��',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�X�j',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�X�j',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�X�j��',2]

TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�L��',2]<-
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�L��',2]+
  TaiwanStudent_clean[TaiwanStudent_clean[,1]=='�L�ץ����',2]
#�g�L�B�z�H���ƪ����K���\�h�ۦP����a�A�A�N�ۦP����a�`�H�ƥ[�_��
TaiwanStudent_clean<-
  TaiwanStudent_clean%>%
  group_by(���Ǯ�.���c.��O.�a��.)%>%
  summarise(�`�H��=sum(�`�H��))%>%
  arrange(desc(�`�H��))
#�N���t�����j���W�٪�row����
TaiwanStudent_clean<-TaiwanStudent_clean[-c(4,8,9,21,35,70),]

TaiwanStudent_clean%>%
  arrange(desc(�`�H��))%>%
  head(10)%>%
  knitr::kable()

TaiwanStudent%>%
  group_by(�ǮզW��)%>%
  summarise(�`�H��=sum(�p�p))%>%
  arrange(desc(�`�H��))%>%
  head(10)

ggplot(TaiwanStudent_clean,aes(���Ǯ�.���c.��O.�a��.,�`�H��))+
  geom_bar(stat = "identity")+
  labs(x="��a",y="�X��d�ǥͤH��",title="��103�~��106�~�X��d�ǥ�")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dataToBAR<-
  rbind(head(TaiwanStudent_clean,33),
      slice(TaiwanStudent_clean,34:n()) %>% 
        summarise(���Ǯ�.���c.��O.�a��.="other",�`�H��=sum(�`�H��))
)
ggplot(dataToBAR,aes(���Ǯ�.���c.��O.�a��.,�`�H��))+
  geom_bar(stat = "identity")+
  labs(x="��a",y="�X��d�ǥͤH��",title="��103�~��106�~�X��d�ǥ�")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#�o�OR Code Chunk
EnglihCountries<-c()
for(i in 1:nrow(TaiwanStudent_clean)){
  for(j in 1:nrow(Comparision)){#�N�n�e���檺��Ƥ��N����a��쪺��ƨ��X�A�ι�Ӫ���إ߹�ӭ^���W
    if(TaiwanStudent_clean[i,"���Ǯ�.���c.��O.�a��."]==
       Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(TaiwanStudent_clean[i,"���Ǯ�.���c.��O.�a��."]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")#�Y�S����Ӫ��^��h�s��Unmatch
  }
}
#�N�谵�n���^���ӦW�٥[�J����
TaiwanStudent_clean$Country<-array(unlist(EnglihCountries))
#�Y�n�Hchoroplethr�@���q�Ϫ��ܥ����O�@�ӥu��region�Mvalue��쪺dataframe
choropleth = data.frame(region=TaiwanStudent_clean$Country, value=TaiwanStudent_clean$�`�H��)
#�]����choroplethr�����q�Ϥ��঳���ƪ���ơA�R�����ƪ����(�]�N�O���Unmatch�����)
choropleth<-choropleth[!duplicated(choropleth$region),]
#�Hcountry level�ӧ@���q��
country_choropleth(choropleth)

Exchangestudent<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
Exchangestudent[,4:6]<-NULL
Exchangestudent$�`�H��<-as.numeric(Exchangestudent$�`�H��)
Exchangestudent%>%
  arrange(desc(�`�H��))%>%
  select('��O','�`�H��')%>%
  head(10)

EnglihCountries<-c()
for(i in 1:29){
  for(j in 1:273){#�N�n�e���檺��Ƥ��N����a��쪺��ƨ��X�A�ι�Ӫ���إ߹�ӭ^���W
    if(Exchangestudent[i,"��O"]==CountriesComparisionTable[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,CountriesComparisionTable[j,"English"])
      break
    }else if(Exchangestudent[i,"��O"]!=CountriesComparisionTable[j,"Taiwan"]&j==272)
      EnglihCountries<-c(EnglihCountries,"Unmatch")#�Y�S����Ӫ��^��h�s��Unmatch
  }
}
#�N�谵�n���^���ӦW�٥[�J����
Exchangestudent$Country<-array(unlist(EnglihCountries))
#�Y�n�Hchoroplethr�@���q�Ϫ��ܥ����O�@�ӥu��region�Mvalue��쪺dataframe
choropleth = data.frame(region=Exchangestudent$Country, value=Exchangestudent$�`�H��)
#�]����choroplethr�����q�Ϥ��঳���ƪ���ơA�R�����ƪ����(�]�N�O���Unmatch�����)
choropleth<-choropleth[!duplicated(choropleth$region),]
#�Hcountry level�ӧ@���q��
country_choropleth(choropleth)

con<-file('CountriesComparisionTable.csv',encoding="UTF-8")
write.csv(CountriesComparisionTable,file=con,row.names = F)


EnglihCountries<-c()
for(i in 1:177){
  for(j in 1:273){
    if(dataToBar[i,"��O"]==Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(dataToBar[i,"��O"]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")
  }
}
dataToBar$Country<-array(unlist(EnglihCountries))
df = data.frame(region=dataToBar$Country, value=dataToBar$�̲��`�H��)
df<-df[!duplicated(df$region),]
country_choropleth(df)+
  scale_fill_brewer(palette=1) 

EnglihCountries<-c()
for(i in 1:nrow(TaiwanStudent_clean)){
  for(j in 1:nrow(Comparision)){#�N�n�e���檺��Ƥ��N����a��쪺��ƨ��X�A�ι�Ӫ���إ߹�ӭ^���W
    if(TaiwanStudent_clean[i,"���Ǯ�.���c.��O.�a��."]==
       Comparision[j,"Taiwan"]){
      EnglihCountries<-c(EnglihCountries,Comparision[j,"English"])
      break
    }else if(TaiwanStudent_clean[i,"���Ǯ�.���c.��O.�a��."]!=Comparision[j,"Taiwan"]&j==273)
      EnglihCountries<-c(EnglihCountries,"Unmatch")#�Y�S����Ӫ��^��h�s��Unmatch
  }
}
#�N�谵�n���^���ӦW�٥[�J����
TaiwanStudent_clean$Country<-array(unlist(EnglihCountries))
#�Y�n�Hchoroplethr�@���q�Ϫ��ܥ����O�@�ӥu��region�Mvalue��쪺dataframe
choropleth = data.frame(region=TaiwanStudent_clean$Country, value=TaiwanStudent_clean$�`�H��)
#�]����choroplethr�����q�Ϥ��঳���ƪ���ơA�R�����ƪ����(�]�N�O���Unmatch�����)
choropleth<-choropleth[!duplicated(choropleth$region),]
#�Hcountry level�ӧ@���q��
country_choropleth(choropleth)+
  scale_fill_brewer(palette=8) 