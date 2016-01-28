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
  output <- gsub('^(Prime for |Prime |Prime - |Prime-)', "", names)
  return(output)
}


map_name_to_acronym <- function(names, mapping) {
  # Maps the complete methodology names to an abbreviated version of the name
  #
  # Args:
  #   names: character vector containing the complete methodology names
  #   mapping: character vector containing the mapping rules. Each entry has
  #            the following form "full name, acronym"
  #         
  # Returns:
  #   Vector with abbreviated methodology names
  
  full_names <- str_extract(mapping, '^.*,') %>%
    str_sub(1, str_length(.) - 1) %>%
    str_trim(side = "both")
  
  acronyms <- str_extract(mapping, ",.*") %>%
    str_sub(2, str_length(.)) %>%
    str_trim(side = "both")
  
  return(mapvalues(names, full_names, acronyms, warn_missing = FALSE))
  
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
      days.planned = sum(days.planned),
      days.spent = sum(days.spent)
    )
  names(oa.tmp)[names(oa.tmp) == 'days.planned'] <- 'oa.days.planned'
  names(oa.tmp)[names(oa.tmp) == 'days.spent'] <- 'oa.days.spent'
  
  lc.tmp <- filter(lc.prime.tasks, tracker == 'Work package') %>%
    select(methodology, lc.issue.numb, subject, done)
  
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
                      oa.days.planned, oa.days.spent, done)
  
  merged.df %<>% replace_na(list(
    oa.days.planned = 0,
    oa.days.spent = 0,
    done = 0
  ))
  
  return(merged.df)
}

get_issue_hierarchy  <- function(lc_data_df, issue_numbers) {
  # Identifies all LC report records belonging to a certain issue hierarchy 
  # giventhe issue numbers from the parent level.
  #
  #
  # Args:
  #   lc.data.df: LC issue list
  #   issue_number: issue numbers from the desisred parent level the search 
  #                 starts at
  #         
  # Returns:
  #   Vector containing issue numbers from the entire issue hierachy
  
  child_issues <- lc_data_df$lc.issue.numb[lc_data_df$parent.task %in% issue_numbers]
  
  if (length(child_issues) > 0) {
    tmp <- get_issue_hierarchy(lc_data_df, child_issues)
    issue_numbers <- c(issue_numbers, tmp)
  }
  return(issue_numbers)
}


# 2. Process OpenAir and LabCase raw data --------------------------------------

# Process Open Air data

oa.voluntary.raw <- read.csv('./rawData/prime_voluntary.csv', 
                             header = TRUE, encoding = 'UTF-8')
oa.billable.raw <- read.csv('./rawData/prime_bookable.csv',
                            header = TRUE, encoding = 'UTF-8')

# Add cost_type column to data frames to separate billable from voluntary 
# work later (V: Voluntary, B: Billable)
oa.voluntary.raw$cost_type <- rep('V', dim(oa.voluntary.raw)[1])
oa.billable.raw$cost_type <- rep('B', dim(oa.billable.raw)[1])

# Process merged OpenAir raw data
names(oa.voluntary.raw) <- tolower(names(oa.voluntary.raw))
names(oa.billable.raw) <- tolower(names(oa.billable.raw))


# Section added to differ between timesheet- and project-based report. 
# Ensures that we won't break the subsequent code below because of column
# name differences between the two report types.
oa.billable.raw$task.planned.hours <- NULL
oa.billable.raw %<>%
  rename(
    approved.actual.cost..eur. = task.project...actual.cost...eur.,
    approved.hours = task.project...approved.hours,
    task.planned.hours = all.assigned.hours
  )

# Merge billable and voluntary project
oa.processed <- rbind(oa.voluntary.raw, oa.billable.raw)

oa.processed %<>% 
  replace_na(list(
    approved.hours = 0
  )) %>%
  rename(
    methodology = phase
  ) %>%
  mutate(
    methodology = cutNamePrefix(methodology),
    methodology = map_name_to_acronym(methodology, config$mapping),
    task = as.character(task),
    user = gsub(" ", "", user),
    days.planned = task.planned.hours / 8,
    days.spent = approved.hours / 8
  )


# Extract total billable and total voluntary hours spent
total.vol.hours <- sum(oa.processed$approved.hours[oa.processed$cost_type == "V"])
total.bill.hours <- sum(oa.processed$approved.hours[oa.processed$costtype == "B"])

# Correct possible false setup in source system
oa.processed$approved.actual.cost..eur.[oa.processed$cost_type == "V"] <- 0

# Extract LC issue number and store result in separate column
oa.processed$lc.issue.numb <- as.numeric(sapply(regmatches(oa.processed$task, 
                                                regexec('^([0-9]+)', 
                                                        oa.processed$task)),
                                     function(x)x[2]))
# Add dummy issue number to OA tasks without LC representation
group_index <- group_indices(oa.processed, methodology, task)
oa.processed$lc.issue.numb[is.na(oa.processed$lc.issue.numb)] <- group_index[is.na(oa.processed$lc.issue.numb)]



# Process LabCase data (employee list)

# Load employee-country-mapping
empl.country.map <- read_excel('./rawData/employeeCountryMapping.xlsx')
names(empl.country.map)[1] <- 'user'

# Merge OpenAir report with employee/country excel file
oa.pro.mer <- merge(oa.processed, empl.country.map, by = c('user'), 
                    all.x = TRUE)



# Process LabCase data (task list)

lc.prime.tasks <- read.csv('./rawData/lc_tasks.csv', 
                           header = TRUE, encoding = 'UTF-8') %>%
  mutate(
    methodology = cutNamePrefix(methodology),
    methodology = map_name_to_acronym(methodology, config$mapping)
  )


template_issues <- lc.prime.tasks %>%
  filter(subject == "[Template - Copy me and enter service package name]") %$%
  lc.issue.numb %>%
  get_issue_hierarchy(lc.prime.tasks, .)

lc.prime.tasks %<>% filter(!lc.issue.numb %in% template_issues)


                                  
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
                                              digits = 1)
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
  daysSpentByContributor <- mutate(daysSpentByContributor, total.days = B + V)
} else {
  daysSpentByContributor$V <- rep(0, nrow(daysSpentByContributor))
  daysSpentByContributor <- mutate(daysSpentByContributor, total.days = B)
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
                                   #achievementInPercent = (sum(spent.time) / sum(estimated.time))*100
                                   achievementInPercent = sum(done) / n()
                                 )

write.csv(releaseProgressByMethodology, 
          file = './rOutput/releaseProgressByMethodolgy.csv', row.names = FALSE)



# 3.2 Create and save .csv files (Details page) --------------------------------

# Create merged work package status table
mergedOaLcWorkPackageStatus <- mergeLcOaWorkPackageData(oa.pro.mer, lc.prime.tasks)

write.csv(mergedOaLcWorkPackageStatus,
          file = './rOutput/megedOaLCWorkPackageStatus.csv', row.names = FALSE)



# Create spent days by contributor table 
oaDaysSpentByContributor <- group_by(oa.pro.mer, methodology, lc.issue.numb, 
                                     user) %>%
  summarize(
    days.planned = sum(days.planned),
    days.spent = sum(days.spent) 
  )

write.csv(oaDaysSpentByContributor, 
          file = './rOutput/oaDaysSpentByContributor.csv', row.names = FALSE)

readinessByPlatform <- lc.prime.tasks %>% 
  filter(subject %in% c("General readiness", "Sales Readiness",
                        "Consulting Readiness")) %>%
  group_by(methodology, subject) %>%
  summarize(
    done = sum(done)/n()
  ) %>%
  spread(subject, done)

write.csv(readinessByPlatform,
          file = "./rOutput/readinessByPlatform.csv", row.names = FALSE)
  
  


