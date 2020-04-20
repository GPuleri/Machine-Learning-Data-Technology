setwd ("D:\\Sgmon\\Documents\\Magistrale\\Data Technology\\progetto")
census=data.frame(read.csv(file="censusFinal.csv",header=TRUE, sep=';', na.string = c("")))

adult=data.frame(read.csv(file="adultFinal.csv",header=TRUE, sep=';', na.string = c("")))
##CENSUS
#computation of age in census
age=census$year-census$YoB
census$age=age
census$year=NULL
census$YoB=NULL

#add column relationship in census
relationship=array(data=NA,dim=dim(census)[1])
relationship[census$det_hh_summ=='householder']='husband'
relationship[census$det_hh_summ=='spouse of householder']='wife'
relationship[census$det_hh_summ=='child 18 or older']='own child'
relationship[census$det_hh_summ=='child under 18 never married']='own child'
relationship[census$det_hh_summ=='child under 18 ever married']='own child'
relationship[census$det_hh_summ=='other relative of householder']='other relative'

relationship[census$det_hh_summ=='group quarters  secondary individual']='unknown'
relationship[is.na(relationship)]='unmarried'
relationship[relationship=='unknown']=NA
relationship=factor(relationship)
census$relationship=relationship
census$det_hh_summ=NULL

#update state_prev_res
levels(census$state_prev_res)=c(levels(census$state_prev_res), 'nonmover')
census$state_prev_res[is.na(census$state_prev_res)]='nonmover'

#fam_under_18 with age>=18 add adult
levels(census$fam_under_18)=c(levels(census$fam_under_18),'adult')
census$fam_under_18[census$age>=18]='adult'

##ADULT
#add fam_under_18
fam_under_18=array(data=NA,dim=dim(adult)[1])
fam_under_18[adult$age >= 18]='adult'
fam_under_18=factor(fam_under_18)
adult$fam_under_18=fam_under_18

#add full_or_part_emp
full_or_part_emp=array(data=NA,dim=dim(adult)[1])
full_or_part_emp[adult$hours_per_week>=35]='full time schedules'
full_or_part_emp[adult$hours_per_week<35]='unemployed part  time'
full_or_part_emp[adult$hours_per_week<=10]='unemployed full time'
full_or_part_emp[adult$occupation=='armed forces']='children or armed forces'
full_or_part_emp[adult$age<18]='children or armed forces'
full_or_part_emp=factor(full_or_part_emp)
adult$full_or_part_emp=full_or_part_emp
adult$hours_per_week=NULL

write.csv2(adult, file = "adultPost.csv", na="",  row.names=FALSE)
write.csv2(census, file = "censusPost.csv", na="",  row.names=FALSE)











  