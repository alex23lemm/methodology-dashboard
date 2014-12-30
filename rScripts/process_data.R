# Purpose: The process.R script loads the previously extracted raw data into 
# memory and processes it. Its major task is to construct tiny data frames which
# will be saved as .csv files. Those .csv files will later be consumed by the
# UI layer to visualize the data.
#
# process.data.R consists of four mayor parts:
#  1. Definition of utitility functions
#  2. Process OpenAir and LabCase raw data
#  3. Create and save .csv files
#

# if statement added because the rJava package referenced by the xlsx package 
# does not work when JAVA_HOME is set on Windows Server 64 Bit 
# see http://bit.ly/10OGlQx (stackoverflow)
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")



# 1. Define utility functions---------------------------------------------------

cutNamePrefix <- function(names) {
  # Cuts the prefix of the OA phase name or the LC project name
  #
  # Args:
  #   names: character vector containing the complete methodology names
  #          including a prefix
  #        
  # 
  # Returns:
  #   Vector with full methodology names
  output <- gsub('^(Prime for |Prime |Prime - |Prime-|2014 Prime for )', "", names)
  return (output)
}


mapToAcronym <- function(names){
  # Maps the complete methodology names to an abbreviated version of the name
  #
  # Args:
  #   names: character vector containing the complete methodology names 
  #         
  # 
  # Returns:
  #   Vector with abbreviated methodology names
  output <- rep(NA, length(names))
  for(i in 1:length(names))
    output[i] <- ifelse(names[i] == 'Process Improvement Methodology', 'General',
          ifelse(names[i] == 'Governance Risk and Compliance', 'GRC',
          ifelse(names[i] == 'Project Management', 'PM',
          ifelse(names[i] == 'Cockpit and Prime-to-Go', 'Ckpt and PtG',
          ifelse(names[i] == 'Project Governance', 'Project Gov',
          ifelse(names[i] == 'Master Data Management', 'MDM',
          ifelse(names[i] == 'Enterprise Integration', 'EI',
          ifelse(names[i] == 'Process Intelligence', 'PI',
          ifelse(names[i] == 'Process-driven SAP Management', 'PDSAP',
          ifelse(names[i] == 'Process-Driven SAP Management', 'PDSAP',
          ifelse(grepl('^Training', names[i]), 'Trng Mgm',
          ifelse(names[i] == 'Business Process Management', 'BPM',
          ifelse(names[i] == 'Business Process Management ', 'BPM',
          ifelse(names[i] == 'Business Process Analysis', 'BPA', 
          ifelse(names[i] == 'Development and Maturity', 'Dev & Maturity',
          ifelse(names[i] == 'Methodology Maturity  and Alignment', 'Alignment',
          ifelse(names[i] == 'No phase assigned', 'No phase',        
          ifelse(names[i] == 'Adoption Support', 'Adopt Supp',       
          ifelse(names[i] == 'Implementation', 'Impl',           
          ifelse(names[i] == 'Enterprise Architecture Management', 'EAM',
          ifelse(grepl('^Model to Execute', names[i]), 'M2E',
          ifelse(grepl('^IPR', names[i]), 'IPR',
          ifelse(names[i] == 'Solution Book Support Activities', 'SB Support', 
          ifelse(names[i] == 'webMethods Upgrades', 'wM Upgrades',
          names[i]))))))))))))))))))))))))
  return (as.factor(output))
}


mergeLcOaWorkPackageData <- function(oa.data.df, lc.data.df) {
  # Merges the LC and OA data on the work package level 
  #
  # Args:
  #   oa.data.df: Processed OA data
  #   lc.data.df: Processed LC data
  #         
  # Returns:
  #   A data frame containing the merged data and selected columns. In addition, 
  #   the column 'check' will show if a data problem exists in one of the source
  #   systems:
  #   0: no issue
  #   1: LC data issue 
  #   2: OA data issue
  
  oa.tmp <- group_by(oa.data.df, methodology, task, lc.issue.numb) %>%
    summarize(
      # Divide by n() (nummber of resources assigend to this task) because the 
      # total sum of days.planned for a specific task is shown for each assigend
      # resource
      days.planned = sum(days.planned)/n(),
      # Unlike for days.planned, the days.spent column shows the days spent for 
      # each individual resource
      days.spent = sum(days.spent)
    )
  names(oa.tmp)[names(oa.tmp) == 'days.planned'] <- 'oa.days.planned'
  names(oa.tmp)[names(oa.tmp) == 'days.spent'] <- 'oa.days.spent'
  
  
  lc.tmp <- filter(lc.prime.tasks, tracker == 'Work package') %>%
    select(methodology, lc.issue.numb, subject, estimated.days, spent.days,
           done)
  names(lc.tmp)[names(lc.tmp) == 'estimated.days'] <- 'lc.days.planned'
  names(lc.tmp)[names(lc.tmp) == 'spent.days'] <- 'lc.days.spent'
  
  
  merged.df <- merge(lc.tmp, oa.tmp, by = 'lc.issue.numb', all = TRUE)
  # Add check column to highlight the source system in which a data problem exists
  # 0: no issue
  # 1: LC issue
  # 2: OA issue
  #m$check <- rep(0, dim(m)[1])
  merged.df$check <- ifelse(is.na(merged.df$subject), 1, 
                    ifelse(is.na(merged.df$task), 2 , 0
                    ))
  merged.df$methodology.x <- ifelse(is.na(merged.df$methodology.x), 
                                         as.character(merged.df$methodology.y),
                                         as.character(merged.df$methodology.x))
  merged.df$subject <- ifelse(is.na(merged.df$subject), 
                                   as.character(merged.df$task), 
                                   as.character(merged.df$subject))
  names(merged.df)[names(merged.df) == 'methodology.x'] <- 'methodology'
  
  merged.df <- select(merged.df, 
                      check, lc.issue.numb, methodology, subject, 
                      lc.days.planned, lc.days.spent, oa.days.planned, 
                      oa.days.spent, done)
  merged.df$lc.days.planned[is.na(merged.df$lc.days.planned)] <- 0
  merged.df$lc.days.spent[is.na(merged.df$lc.days.spent)] <- 0
  merged.df$oa.days.planned[is.na(merged.df$oa.days.planned)] <- 0
  merged.df$oa.days.spent[is.na(merged.df$oa.days.spent)] <- 0
  merged.df$done[is.na(merged.df$done)] <- 0
  
  return (merged.df)
}



# 2. Process OpenAir and LabCase raw data --------------------------------------

# Process Open Air data

oa.voluntary.raw <- read.csv('./rawData/prime_voluntary.csv', 
                             header=TRUE, encoding='UTF-8')
oa.billable.raw <- read.csv('./rawData/prime_bookable.csv',
                            header=TRUE, encoding='UTF-8')

# Extract total billable and total voluntary hours spent
total.vol.hours <- sum(oa.voluntary.raw$Approved.hours)
total.bill.hours <- sum(oa.billable.raw$Approved.hours)

# Add cost_type column to data frames to separate billable from voluntary 
# work later (V: Voluntary, B: Billable)
oa.voluntary.raw$cost_type <- rep('V', dim(oa.voluntary.raw)[1])
oa.billable.raw$cost_type <- rep('B', dim(oa.billable.raw)[1])

# Correct possible false setup in source system
oa.voluntary.raw$Approved.actual.cost..EUR. <- 0


# Merge billable and voluntary project
oa.processed <- rbind(oa.voluntary.raw, oa.billable.raw)

# Process merged OpenAir raw data
names(oa.processed) <- tolower(names(oa.processed))
names(oa.processed)[names(oa.processed) == 'phase'] <- 'methodology'
oa.processed$methodology <- cutNamePrefix(oa.processed$methodology)
oa.processed$methodology <- mapToAcronym(oa.processed$methodology)

oa.processed <- transform(oa.processed,
                          task = as.character(task),
                          user = gsub(" ", "", user),
                          days.planned = task.planned.hours / 8,
                          days.spent = approved.hours / 8)

# Extract LC WP number and store result in separate column
oa.processed$lc.issue.numb <- as.numeric(sapply(regmatches(oa.processed$task, 
                                                regexec('WP ([0-9]+)', 
                                                        oa.processed$task)),
                                     function(x)x[2]))




# Process LabCase data (employee list)

# Load employee-country-mapping
empl.country.map <- read.xlsx('./rawData/employeeCountryMapping', 
                            sheetIndex = 1, encoding = 'UTF-8')
names(empl.country.map)[1] <- 'user'

# Merge OpenAir report with employee/country excel file
oa.pro.mer <- merge(oa.processed, empl.country.map, by = c('user'), all.x=TRUE)



# Process LabCase data (project list)

lc.prime.tasks <- read.xlsx('./rawData/lcPrimeTasks.xls',
                            sheetIndex = 1, encoding = 'UTF-8')
names(lc.prime.tasks) <- tolower(names(lc.prime.tasks))
names(lc.prime.tasks)[names(lc.prime.tasks) == 'x..done'] <- 'done'
names(lc.prime.tasks)[names(lc.prime.tasks) == 'project'] <- 'methodology'
names(lc.prime.tasks)[names(lc.prime.tasks) == 'x.'] <- 'lc.issue.numb'
lc.prime.tasks$x..1 <- NULL
lc.prime.tasks$methodology <- cutNamePrefix(lc.prime.tasks$methodology)
lc.prime.tasks$methodology <- mapToAcronym(lc.prime.tasks$methodology)


lc.prime.tasks <- mutate(lc.prime.tasks,
                            subject = as.character(subject),
                            estimated.time = as.numeric(as.character(estimated.time)),
                            done = as.numeric(as.character(done)),
                            spent.time = (estimated.time * done) / 100,
                            estimated.days = estimated.time / 8, 
                            spent.days = spent.time / 8,
                            lc.issue.numb = as.numeric(as.character(lc.issue.numb))
                            )
lc.prime.tasks$estimated.time[is.na(lc.prime.tasks$estimated.time)] <- 0
lc.prime.tasks$spent.time[is.na(lc.prime.tasks$spent.time)] <- 0
lc.prime.tasks$estimated.days[is.na(lc.prime.tasks$estimated.days)] <- 0
lc.prime.tasks$spent.days[is.na(lc.prime.tasks$spent.days)] <- 0

                            
                                  
# 3.1 Create and save .csv files (Overview page) -------------------------------

# Calculate total investment in person days per methodology
totalInvestByMethInPersonDays <- oa.pro.mer %>%
                                   group_by(methodology) %>%
                                   summarize(
                                     daysSpent = round(sum(days.spent), 
                                                       digits = 1)
                                     )
write.csv(totalInvestByMethInPersonDays, 
          file = './rOutput/totalInvestByMethInPersonDays.csv', 
          row.names = FALSE)



# Calculate total investment in K euros per methodology
totalInvestByMethInEuros <- oa.pro.mer %>%
                              group_by(methodology) %>%
                              summarize(
                                eurosSpent = round(sum(approved.actual.cost..eur.)/1000,
                                                   digits = 1)
                                )
write.csv(totalInvestByMethInEuros, 
          file = './rOutput/totalInvestByMethInEuros.csv', row.names = FALSE)



# Calculate total investment in person days per methododoly per country
totalInvestByCountry <- oa.pro.mer %>%
                          group_by(methodology, country) %>%
                          summarize(
                            daysSpent = round(sum(days.spent),
                                              digits=1)
                            )
write.csv(totalInvestByCountry, 
          file = './rOutput/totalInvestByCountryInPersonDays.csv', 
          row.names = FALSE)



# Create contributers by country table
daysSpentByContributor <- oa.pro.mer %>%
                            select(user, methodology, country, days.spent, 
                                   cost_type)
# Transform long data to wide data
daysSpentByContributor <- dcast(daysSpentByContributor, 
                                user + methodology + country ~ cost_type, sum, 
                                value.var = "days.spent")
# Add total.days.spent column to data frame
# Check if volunatary work exists
if ("V" %in% names(daysSpentByContributor)) {
  daysSpentByContributor <- transform(daysSpentByContributor, total.days = B + V)
} else {
  daysSpentByContributor <- transform(daysSpentByContributor, total.days = B)
}
# Duplicate methodolgy column in order to have another filter option available 
# on the UI layer
daysSpentByContributor$additional_meth <- daysSpentByContributor$methodology
write.csv(daysSpentByContributor, 
          file = './rOutput/daysSpentByContributor.csv', row.names = FALSE)



# Create total days table
totalVolDays = round(total.vol.hours/8, digits = 1)
totalBillDays = round(total.bill.hours/8, digits = 1)
totalDays <- as.data.frame(cbind(totalDays = totalVolDays + totalBillDays,
                                 totalVolDays, totalBillDays))
write.csv(totalDays, file = './rOutput/totalDays.csv', row.names = FALSE)



# Create release progress by methodology table
# Only include work package trackers; calculate overall methodology achievement 
# in percent
releaseProgressByMethodology <- lc.prime.tasks %>%
                                 filter(tracker == 'Work package') %>%
                                 group_by(methodology) %>%
                                 summarize(
                                   achievementInPercent = (sum(spent.time) / sum(estimated.time))*100
                                 )

write.csv(releaseProgressByMethodology, 
          file='./rOutput/releaseProgressByMethodolgy.csv', row.names=FALSE)



# 3.2 Create and save .csv files (Details page) --------------------------------

# Create merged work package status table
mergedOaLcWorkPackageStatus <- mergeLcOaWorkPackageData(oa.pro.mer, lc.prime.tasks)

write.csv(mergedOaLcWorkPackageStatus,
          file = './rOutput/megedOaLCWorkPackageStatus.csv', row.names = FALSE)

# Create spent days by contributor table 
oaDaysSpentByContributor <- group_by(oa.pro.mer, methodology, lc.issue.numb, 
                                     user) %>%
  summarize(
    days.spent = sum(days.spent) 
  )

write.csv(oaDaysSpentByContributor, 
          file = './rOutput/oaDaysSpentByContributor.csv', row.names = FALSE)




