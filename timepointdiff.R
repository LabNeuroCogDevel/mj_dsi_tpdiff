library(dplyr)
library(tidyr)

#df <- read.table('20161221_combined_rsfMRI_dsi_10_25_final.txt')
df <- read.table('20161221_combined_rsfMRI_dsi_10_25_final(1).txt')

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
 select(-ID,-date) %>% 
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
