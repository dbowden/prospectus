#### Create General Panel Data ####
library(dplyr)

split <- read.csv("~/Dropbox/Prospectus/Joiners/splinter_panel.csv")

#create conflict age
split$conflict.year <- split$year - split$styr

#create starts for specific types
split$troops.start <- ifelse(split$sup.start==1 & split$troops==1, 1, 0)
split$mil.start <- ifelse(split$sup.start==1 & (split$operations==1 | split$territory==1 | split$weapons==1 | split$materiel==1 | split$training==1 | split$intelligence==1), 1, 0)
split$fund.start <- ifelse(split$sup.start==1 & split$funding==1, 1, 0)

split$ext.sup.start <- ifelse(split$sup.start==1 & split$year > split$group.start, 1, 0)
split$ext.mil.start <- ifelse(split$mil.start==1 & split$year > split$group.start, 1, 0)
split$new.sup.start <- ifelse(split$sup.start==1 & split$year==split$group.start, 1, 0)

#code whether it is a new episode
split <- split %>% 
  group_by(ucdpid) %>% 
  mutate(lag.year=lag(year),lag.dyads=lag(dyads),lag.sup.start=lag(sup.start),lag2.sup.start=lag(sup.start,2))

split$new.ep <- ifelse((split$year - split$lag.year)>1, 1, 0)

#create episode numbers
split <- split %>% 
  group_by(ucdpid) %>% 
  mutate(ep.num=cumsum(new.ep))

split$ep.num <- paste(split$ucdpid,split$ep.num,sep="-")

split$ep.year <- 1

split <- split %>% 
  group_by(ep.num) %>% 
  mutate(ep.year=cumsum(ep.year))


#code the splinter variable
split$split.year <- ifelse(split$split.source==1 & split$yearly.split.start>0, 1, 0)

#### Create Country-Year Data ####
cy <- split %>% 
  group_by(ucdpid,year) %>% 
  summarize(dyads=n_distinct(dyadid),split.start=sum(split.start),group.entries=sum(year==group.start),ep.entries=sum(year==styr),ext.sup.start=sum(ext.sup.start),sup.start=sum(sup.start),troops.start=sum(troops.start),mil.start=sum(mil.start),diamonds=max(diamond.sites),drugs=max(drug.sites),gold=max(gold.sites),oil=max(oil.sites),polity=max(polity),area=max(area),pop=max(pop),gdp=max(gdp),gdppc=max(gdppc),fatal=max(BdBest),rebciv.fatal=sum(reb.civ.fatal),new.ep=max(new.ep),ep.year=max(ep.year),non.prev.entries=sum(non.prev.entries),entries=sum(entries),ext.mil.start=sum(ext.mil.start),epr_groups=max(epr_groups),epr_discriminated=max(epr_discriminated),cown=first(cown))

cy <- cy %>% 
  group_by(ucdpid) %>% 
  mutate(lext.sup.start=lag(ext.sup.start),l2ext.sup.start=lag(ext.sup.start,2),ldyads=lag(dyads),lext.mil.start=lag(ext.mil.start),l2ext.mil.start=lag(ext.mil.start,2))

cy <- subset(cy, year>=1975)

cy$ext.sup.start[is.na(cy$ext.sup.start)] <- 0
cy$ext.mil.start[is.na(cy$ext.mil.start)] <- 0
cy$entries[is.na(cy$entries)] <- 0
cy$non.prev.entries[is.na(cy$non.prev.entries)] <- 0

cy <- subset(cy,!is.na(ucdpid))