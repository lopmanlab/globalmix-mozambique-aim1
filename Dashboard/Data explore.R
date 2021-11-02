###making a box plot of the distribution of contacts for day 1 only by age group (Aim1+Aim2)
#run the Local-GMixDash.Rmd first to get the required objects 
ex_hh_ind_all = merge(ex_hh , ind, by = "rec_id")
ex_diaries_all = merge(ex_hh_ind_all, diaries, by = "rec_id")

x = ex_diaries_all %>% dplyr::select(study_site, study_sitelb,genderlb,num_contact_aim1_d1, num_contact_aim2_d1, agelb)%>%
  mutate(num_contact_aim1_d1 = as.numeric(num_contact_aim1_d1),
         num_contact_aim2_d1 = as.numeric(num_contact_aim2_d1))
x$num_contact_aim1_d1[which(is.na(x$num_contact_aim1_d1))] <- 0
x$num_contact_aim2_d1[which(is.na(x$num_contact_aim2_d1))] <- 0
x$num_contact_aim1and2_d1 <- x$num_contact_aim1_d1 + x$num_contact_aim2_d1

summary(x$num_contact_aim1and2_d1[which(x$agelb %in% c('< 6months', '6 - 11months', '1 - 4y'))])
sd(x$num_contact_aim1and2_d1[which(x$agelb %in% c('< 6months', '6 - 11months', '1 - 4y'))])
summary(x$num_contact_aim1and2_d1[which(x$agelb %in% c('< 6months', '6 - 11months', '1 - 4y') & x$study_site=="1")])
sd(x$num_contact_aim1and2_d1[which(x$agelb %in% c('< 6months', '6 - 11months', '1 - 4y')& x$study_site=="1")])
summary(x$num_contact_aim1and2_d1[which(x$agelb %in% c('< 6months', '6 - 11months', '1 - 4y') & x$study_site=="2")])
sd(x$num_contact_aim1and2_d1[which(x$agelb %in% c('< 6months', '6 - 11months', '1 - 4y')& x$study_site=="2")])

x = x[!is.na(x$num_contact_aim1and2_d1),]

lbc = c("0 contacts","1 to 3 contacts","4 to 6 contacts","7 to 10 contacts","11 to 15 contacts","> 15 contacts")

x = x %>% dplyr::mutate(mx = case_when(num_contact_aim1_d1 <1 ~ lbc[1],
                                       num_contact_aim1_d1 >=1 & num_contact_aim1_d1 <=3 ~ lbc[2],
                                       num_contact_aim1_d1 > 3 & num_contact_aim1_d1 <=6 ~ lbc[3],
                                       num_contact_aim1_d1 > 6 & num_contact_aim1_d1 <=10 ~ lbc[4],
                                       num_contact_aim1_d1 > 10 & num_contact_aim1_d1 <=15 ~ lbc[5],
                                       num_contact_aim1_d1 > 15 ~ lbc[6]))

x$mx = factor(x$mx, levels = 1:6, labels = lbc[1:6])

x = x %>% dplyr::group_by(agelb) 

x = x[-which(x$num_contact_aim1and2_d1==112),] #delete the skew value of 112 contact number
x %>% plotly::plot_ly(x=~agelb, y=~num_contact_aim1and2_d1, color=~study_sitelb, colors=colorRamp(c('grey','black')), type = 'box') %>%
  plotly::layout(xaxis = list(title= 'Number of contacts Day 1 from participants by age group'), yaxis = list(title='Total number of contacts'))
x %>% ggbarplot(x='agelb', y='num_contact_aim1and2_d1', color='study_sitelb', palette=c("#00AFBB", "#E7B800"),
                position = position_dodge(0.9), label = T, add = "median_iqr",
                ylab = "Total number of contacts", xlab = "Number of contacts Day 1 from participants by age group")
length(x$study_site[x$study_site=="1"])
length(x$study_site[x$study_site=="2"])
x %>% ggplot(aes(x=agelb, y=num_contact_aim1and2_d1, fill=study_sitelb)) + 
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text=element_text(size=14), axis.title =element_text(size=16),
        legend.position ='top', legend.text =element_text(size=14), legend.title =element_text(size=14) ) +
  scale_fill_discrete(name='Study Site', labels=c('Rural - Manhiça (N=640)', 'Urban - Polana Caniço (N=277)')) +
  labs(x='Number of contacts Day 1 from participants by age group',y='Total number of contacts')


#compare reported cont_num and reported/recorded cont_num
cont_d1_distict_id <- cont_d1 %>%
  group_by(rec_id) %>%
  summarise(num_contact_d1=n())

cont_d2_distict_id <- cont_d2 %>%
  group_by(rec_id) %>%
  summarise(num_contact_d2=n())

cont_diray_d1 <- cont_d1_distict_id %>%
  full_join(diary_1, by="rec_id")
  

cont_diray_d2 <- cont_d2_distict_id %>%
  full_join(diary_2, by="rec_id")

sum(as.numeric(cont_diray_d1$num_contact_aim1_d1) == cont_diray_d1$num_contact_d1, na.rm=T) #87
sum((as.numeric(cont_diray_d1$num_contact_aim1_d1)+as.numeric(cont_diray_d1$num_contact_aim2_d1)) == cont_diray_d1$num_contact_d1, na.rm=T) #0

sum(as.numeric(cont_diray_d2$num_contact_aim1_d2) == cont_diray_d2$num_contact_d2, na.rm=T) #22
sum((as.numeric(cont_diray_d2$num_contact_aim1_d2)+as.numeric(cont_diray_d2$num_contact_aim2_d2)) == cont_diray_d2$num_contact_d2, na.rm=T) #0

cont_diray_d1$diff_cont_d1 <- as.numeric(cont_diray_d1$num_contact_aim1_d1) - cont_diray_d1$num_contact_d1
cont_diray_d2$diff_cont_d2 <- as.numeric(cont_diray_d2$num_contact_aim1_d2) - cont_diray_d2$num_contact_d2
summary(cont_diray_d1$diff_cont_d1)
summary(cont_diray_d2$diff_cont_d2)

#make the plot of the comparison of reported and actual cont_num
cont_diray_d1 <- cont_diray_d1 %>%
  left_join(diary_1_a1[,c('rec_id','agelb','study_sitelb','sexlb')], by="rec_id")
  
cont_diray_d2 <- cont_diray_d2 %>%
  left_join(diary_2_a1[,c('rec_id','agelb','study_sitelb','sexlb')], by="rec_id")

ggplot(data=cont_diray_d1[!is.na(cont_diray_d1$agelb),] %>% gather(Variable, Value) %>% filter(Variable %in% c('num_contact_d1','num_contact_aim1_d1')),
       aes(x=Variable, y=as.numeric(Value), fill=Variable)) + 
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(), axis.text=element_text(size=14), axis.title =element_text(size=16),
        legend.position ='top', legend.text =element_text(size=14), legend.title =element_text(size=14) ) +
  scale_fill_discrete(name='Cont Num', labels=c('Reported', 'Recored')) +
  labs(x='Number of contacts Day 1, reported vs. recored',y='Total number of contacts')

library(reshape2)
dfm <- melt(cont_diray_d1[,c('num_contact_d1','num_contact_aim1_d1','study_sitelb')], id.vars = 3)  
ggplot(data=dfm[!is.na(dfm$study_sitelb),], aes(x=variable, y=as.numeric(value), fill=study_sitelb))+
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(), axis.text=element_text(size=14), axis.title =element_text(size=16),
        legend.position ='top', legend.text =element_text(size=14), legend.title =element_text(size=14) ) +
  #scale_fill_discrete(name='Cont Num', labels=c('Reported', 'Recored')) +
  labs(x='Number of contacts Day 1, reported vs. recored',y='Total number of contacts')

dfm2 <- melt(cont_diray_d1[,c('num_contact_d1','num_contact_aim1_d1','agelb')], id.vars = 3)  
ggplot(data=dfm2[!is.na(dfm2$agelb),], aes(x=variable, y=as.numeric(value), fill=agelb))+
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(), axis.text=element_text(size=14), axis.title =element_text(size=16),
        legend.position ='top', legend.text =element_text(size=14), legend.title =element_text(size=14) ) +
  #scale_fill_discrete(name='Cont Num', labels=c('Reported', 'Recored')) +
  labs(x='Number of contacts Day 1, reported vs. recored',y='Total number of contacts')

dfm3 <- melt(cont_diray_d1[,c('num_contact_d1','num_contact_aim1_d1','agelb')], id.vars = 3)  
ggplot(data=dfm3[!is.na(dfm3$agelb),], aes(x=agelb, y=as.numeric(value), fill=variable))+
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text=element_text(size=14), axis.title =element_text(size=16),
        legend.position ='top', legend.text =element_text(size=14), legend.title =element_text(size=14) ) +
  scale_fill_discrete(name='Cont Num', labels=c('Recored','Reported')) +
  labs(x='Number of contacts Day 1, reported vs. recored',y='Total number of contacts')

dfm4 <- melt(cont_diray_d1[,c('num_contact_d1','num_contact_aim1_d1','sexlb')], id.vars = 3)  
ggplot(data=dfm4[!is.na(dfm4$sexlb),], aes(x=sexlb, y=as.numeric(value), fill=variable))+
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text=element_text(size=14), axis.title =element_text(size=16),
        legend.position ='top', legend.text =element_text(size=14), legend.title =element_text(size=14) ) +
  scale_fill_discrete(name='Cont Num', labels=c('Recored','Reported')) +
  labs(x='Number of contacts Day 1, reported vs. recored',y='Total number of contacts')

dfm5 <- melt(cont_diray_d1[,c('num_contact_d1','num_contact_aim1_d1','study_sitelb')], id.vars = 3)  
ggplot(data=dfm5[!is.na(dfm5$study_sitelb),], aes(x=study_sitelb, y=as.numeric(value), fill=variable))+
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text=element_text(size=14), axis.title =element_text(size=16),
        legend.position ='top', legend.text =element_text(size=14), legend.title =element_text(size=14) ) +
  scale_fill_discrete(name='Cont Num', labels=c('Recored','Reported')) +
  labs(x='Number of contacts Day 1, reported vs. recored',y='Total number of contacts')
###########Summaried Data##########################
aim1_id <- data %>% 
  filter(aim==0) %>% 
  #filter(dirio_de_contato_dia_1_complete==2) %>% 
  group_by(rec_id) %>% 
  summarise(n_aim1=n())

aim2_id <- data %>% 
  filter(aim==1) %>% 
  #filter(dirio_de_contato_dia_1_complete==2) %>% 
  group_by(rec_id) %>% 
  summarise(n_aim2=n())

diray_complete <- data %>% 
  filter(dirio_de_contato_dia_1_complete==2 | dirio_de_contato_dia_2_complete==2) %>%
  group_by(rec_id) %>% 
  summarise(n_cont=n())

intersect(aim1_id$rec_id, diray_complete$rec_id) 
intersect(diray_complete$rec_id, aim1_id$rec_id)

intersect(aim2_id$rec_id, diray_complete$rec_id) 
intersect(diray_complete$rec_id, aim2_id$rec_id)

diray_aim1_by_site <- data %>% 
  filter(rec_id %in% intersect(aim1_id$rec_id, diray_complete$rec_id))

diray_aim2_by_site <- data %>% 
  filter(rec_id %in% intersect(aim2_id$rec_id, diray_complete$rec_id))

diray_aim1and2_by_site <- diray_aim1_by_site %>% 
  inner_join(diray_aim2_by_site, by="rec_id") %>% 
  select(study_site.x, study_site.y)

sum(diray_aim1_by_site$rec_id %in% diray_aim2_by_site$rec_id)

intersect(intersect(aim1_id$rec_id, diray_complete$rec_id), intersect(aim2_id$rec_id, diray_complete$rec_id))

names(data)[which(str_detect(names(data),'complete'))] 

which(!is.na(data$hh_member_sensor))
hh_sensor <- data %>% filter(!is.na(hh_member_sensor)&(hh_member_sensor != 0)) %>% group_by(hh_id) %>% select(hh_id, hh_member_sensor)


which(!is.na(data$sensor_id))
ind_sensor <- data %>% filter(!is.na(sensor_id) & !(sensor_id %in% c('0','00','2','o'))) %>% 
  group_by(rec_id) %>% 
  select(rec_id, sensor_id,hh_id, hh_member_sensor)

which(!is.na(data$hh_id))
length(unique(data$hh_id[which(data$aim==1)]))
length(unique(data$hh_id[which(data$aim==1 & data$study_site==1)]))
length(unique(data$hh_id[which(data$aim==1 & data$study_site==2)]))
unique(data$hh_id[which(data$aim==1 & data$study_site==2)]) %in% unique(data$hh_id[which(data$aim==1)])
unique(data$hh_id[which(data$aim==1 & data$study_site==1)]) %in% unique(data$hh_id[which(data$aim==1)])







