library(dplyr)
library(tidyr)
library(lubridate)

df <- read.table('20161221_combined_rsfMRI_dsi_10_25_final.txt',header=T) 

# fix date 
df[df$id == 10820 & df$date == 20211025,'date'] <- 20121025
df$sex[df$id == 10711 ] <- 1 # is a male

# remove dropped(?) visit with tranposed date 2011-09
df <- df[-which(df$ID=='10765_20091116'),]

# add dob
df <- df %>% mutate(dob = ymd(date) - days(round(age*365.25))) 

## check input
# id and dates match
bad <- df %>% filter(id != gsub('_.*','',ID) | date != gsub('.*_','',ID) ) %>% select(ID,id,date)
if( nrow(bad) > 0L) {print(bad);stop('bad input: mismatch id date')}

# date of births match
bad.dob <- df %>%
 group_by(id) %>%
 summarise(ndob=length(unique(dob)),doboff=length(which(abs(diff(unique(dob)))>5))) %>%
 filter(doboff>0)
if( nrow(bad.dob) > 0L) {print(bad.dob);stop('bad input: bad dob')}

# sex match
bad.sex <- 
 df %>%
 group_by(id) %>%
 summarise(nsex=length(unique(na.omit(sex)))) %>%
 filter(nsex!=1)
if( nrow(bad.sex) > 0L) {print(bad.sex);stop('bad input: conflicting sex')}

replacedata.mean <- df %>%
 group_by(id) %>%
 summarise(dob=first(dob) + days(round(mean(c(0,diff(na.omit(dob))),na.rm=T))), sex=mean(sex,na.rm=T),n=n())

# guess age where missing 
naagei <- is.na(df$age)
rpli <- replacedata.mean$id %in% df$id[naagei]
df$age[naagei] <- (ymd(df$date[naagei]) - replacedata.mean$dob[rpli])/365.25

# guess sex where missing
nai <- is.na(df$sex)
rpli <- replacedata.mean$id %in% df$id[nai]
df$sex[nai] <- replacedata.mean$sex[rpli]


##############################

# use rank to get/set visit time point
# hang out to number of visits "ntp" and max visit number "mxtp" for QA
d.r <- 
 df %>% 
 arrange(id,date) %>%
 group_by(id) %>% 
 mutate(tp=rank(date),mxtp=max(tp),ntp=length(unique(date)) ) %>% 
 filter(mxtp==ntp) %>% # remove 10709_2011080 confusion
 # make timepoint values a friendly name so it can easily be a column when it grows up
 mutate(tp=sprintf('t%d',tp))

# make long (all measuers in one column)
# and pivot on time (column for each time point)
d.s <- 
 d.r %>%
 # remove if haven't seen more than once
 filter(mxtp>1)  %>% 
 # remove date/time columns -- want tp as sole time column
 # so we can pivot on time later -- wont work if non-unique pivot point
 select(-ID,-date,-ntp,-dob) %>% 
 # put all the measure columns into long fmt
 gather('meas','val',-id,-sex,-tp,-mxtp) %>% 
 # put timepoint as columns
 spread(tp,val) 

# get timepoint diffs
d.td <- 
 d.s %>% 
 mutate(t21=t2-t1, t32=t3-t2)

# put back into a wide format
# column per measure_timepoint
d.out <-
 d.td %>%
 select(id,sex,meas,t21,t32)  %>%
 gather('tp','diff',-id,-sex,-meas) %>% 
 unite(meastp,meas,tp) %>% 
 spread(meastp,diff)
 #reshape2::dcast(id+sex~meas,value.var=c('t32','t21'))


# save
write.table(d.out,
  file="20161221_combined_rsfMRI_dsi_10_25_final_timediff.txt",
  row.names=F,quote=F)
