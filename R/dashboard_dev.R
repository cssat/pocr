

library(RODBC)
library(stringr)
library(pocr)

annie <- odbcConnect("annie")
name <- c('data', 'label')

# DASHBOARD 1 TAB
# opened iunvestigations comes from most recent count of invesitations and assessments

sp_investigations <- stored_procedure('ia_trends_counts')
query_investigations <- sqlQuery(annie, sp_investigations)
clean_investigations <- cr_clean(query_investigations, date.type = 2)

investigations <- filter(clean_investigations, date == max(date)) %>%
    select(- closed.investigations.and.assessments)

investigations_dat <- format(investigations$opened.investigations.and.assessments, big.mark = ',')
investigations_label <- "investigations into reported child abuse or neglect"

investigations <- data.frame(investigations_dat, investigations_label)
names(investigations) <- name

# INVESTIGATIONS DATA
# RACE/ETHNICITY

sp_invest_raceeth <- stored_procedure('ia_trends_rates', ethnicity = c(1, 3, 5, 8, 9))
query_invest_raceeth <- sqlQuery(annie, sp_invest_raceeth)
clean_invest_raceeth <- cr_clean(query_invest_raceeth, select = 'ethnicity_cd', date.type = 2)

filt_invest_raceeth <- filter(clean_invest_raceeth, date == max(date)) %>% select(race.ethnicity, opened.investigations.and.assessments)

# REPORTERS

sp_invest_report <- stored_procedure('ia_trends_counts', reporter = c(1:14))
query_invest_report <- sqlQuery(annie, sp_invest_report)
clean_invest_report <- cr_clean(query_invest_report , select = 'cd_reporter_type', date.type = 2)

filt_invest_report <- filter(clean_invest_report, date == max(date)) %>%
    mutate(count = sum(opened.investigations.and.assessments)) %>%
    group_by(reporter.desc) %>%
    summarize(percent = round((opened.investigations.and.assessments/count) * 100)) %>%
    filter(reporter.desc == 'Social Service Professional'| 
           reporter.desc == 'Educator'| 
           reporter.desc == 'Parent/Guardian'| 
           reporter.desc == 'Law Enforcement Officer')

reporter.desc <- 'Other'
percent <- 100 - sum(filt_invest_report$percent)
other_rep <- data.frame(reporter.desc, percent)

reporters <- rbind(filt_invest_report, other_rep)

# example
# prettify(toJSON(list(graphtype = "donut", data = reporters)))

# REASONS

sp_invest_reason <- stored_procedure('ia_trends_counts', allegation = c(1:3))
query_invest_reason <- sqlQuery(annie, sp_invest_reason)
clean_invest_reason <- cr_clean(query_invest_reason, select = 'cd_allegation', date.type = 2)

filt_invest_reason <- filter(clean_invest_reason, date == max(date)) %>%
    mutate(reports = opened.investigations.and.assessments) %>%
    select(allegation, reports) %>%
    arrange(allegation)

# DASHBOARD 2 TAB
# count of children in out of home care

sp_count_ooh <- stored_procedure('ooh_pit_counts')
query_count_ooh <- sqlQuery(annie, sp_count_ooh)
clean_count_ooh <- cr_clean(query_count_ooh)

count_ooh <- filter(clean_count_ooh, date == max(date))

count_data <- format(count_ooh$total.in.out.of.home.care.1st.day, big.mark = ',')
count_label <- paste('children in out-of-home care on', as.character(pretty_date(count_ooh$date)))

count <- data.frame(count_data, count_label)
names(count) <- name

# OOH
# RACE/ETHNICITY

sp_ooh_raceeth <- stored_procedure('ooh_pit_rates', ethnicity = c(1, 3, 5, 8, 9))
query_ooh_raceeth <- sqlQuery(annie, sp_ooh_raceeth)
clean_ooh_raceeth <- cr_clean(query_ooh_raceeth, select = 'ethnicity_cd', date.type = 2)

filt_ooh_raceeth <- filter(clean_ooh_raceeth, date == max(date)) %>% select(race.ethnicity, total.in.out.of.home.care.1st.day)

# AGE IN CARE

sp_ooh_age <- stored_procedure('ooh_pit_count', age = c(1:4))
query_ooh_age <- sqlQuery(annie, sp_ooh_age)
clean_ooh_age <- cr_clean(query_ooh_age , select = 'age_grouping_cd', date.type = 2)

ooh_age <- filter(clean_ooh_age, date == max(date)) %>%
            mutate(count = sum(total.in.out.of.home.care.1st.day)) %>%
            group_by(age.grouping) %>%
            summarize(percent = round((total.in.out.of.home.care.1st.day/count) * 100))

ooh_age$age.grouping <- str_trim(str_replace_all(str_replace_all(ooh_age$age.grouping, ' through ', '-'), '[a-z(.*)]|[A-Z(.*)]|\\(|\\)', ''))
ooh_age$percent <- paste0(ooh_age$percent, '%')

# COUNTY HIGHLIGHTS

# getting the 4 counties with the most kids in ooh
sp_ooh_county_count <- stored_procedure('ooh_pit_count', county = c(1:34))
query_ooh_county_count <- sqlQuery(annie, sp_ooh_county_count)
clean_ooh_county_count <- cr_clean(query_ooh_county_count, select = 'county_cd', date.type = 2)

high_cnt_county <- filter(clean_ooh_county_count, date == max(date)) %>% arrange(-total.in.out.of.home.care.1st.day) %>% slice(1:4) %>% select(county.cd)

sp_ooh_county <- stored_procedure('ooh_pit_rates', county = high_cnt_county[, 1])
stored_procedure('ooh_pit_rates', county = c(test))
query_ooh_county <- sqlQuery(annie, sp_ooh_county)
clean_ooh_county <- cr_clean(query_ooh_county, select = 'county_cd', date.type = 2)

ooh_county <- filter(clean_ooh_county, date == max(date)) %>% 
    mutate(rate = total.in.out.of.home.care.1st.day) %>% 
    arrange(-rate) %>%
    select(county, rate)

# DASHBOARD 3 TAB
# outcomes

sp_outcomes <- stored_procedure('ooh_outcomes')
query_outcomes <- sqlQuery(annie, sp_outcomes)
clean_outcomes <- cr_clean(query_outcomes, date = F)

outcomes_date <- filter(clean_outcomes, percent < 50, cd.discharge.type == 0) %>% 
    filter(cohort.period == max(cohort.period)) %>% 
    select(cohort.period) 

min_dat <- filter(clean_outcomes, cohort.period == as.numeric(outcomes_date), cd.discharge.type == 0, percent <= 50) %>%
    filter(percent == max(percent)) 

max_dat <- filter(clean_outcomes, cohort.period == as.numeric(outcomes_date), cd.discharge.type == 0, percent >= 50) %>%
    filter(percent == min(percent)) 

outcomes_dat <- round(approx(x = c(min_dat$percent, max_dat$percent), y = c(min_dat$months.since.entering.out.of.home.care, max_dat$months.since.entering.out.of.home.care), xout = 50)$y)
outcomes_dat <- paste(outcomes_dat, 'Months')

outcomes_label <- "median length of stay in out-of-home care"

outcomes <- data.frame(outcomes_dat, outcomes_label)
names(outcomes) <- name

# Highlights

recent_date <- filter(clean_outcomes, months.since.entering.out.of.home.care == 24, cd.discharge.type == 0) %>% 
    filter(cohort.period == max(cohort.period)) %>%
    select(cohort.period)

recent_data <- filter(clean_outcomes, cohort.period == recent_date[1,], cd.discharge.type != 6)
recent_data$discharge <- str_replace(recent_data$discharge, 'Out-of-Home ', '')

# 6 MONTHS

six_months <- filter(recent_data, months.since.entering.out.of.home.care == 6) %>%
    select(discharge, percent)

six_months$percent <- paste0(six_months$percent, '%')

# 1 YEAR

one_year <- filter(recent_data, months.since.entering.out.of.home.care == 12) %>%
    select(discharge, percent)

one_year$percent <- paste0(one_year$percent, '%')

# 2 YEARS

two_years <- filter(recent_data, months.since.entering.out.of.home.care == 24) %>%
    select(discharge, percent)

two_years$percent <- paste0(two_years$percent, '%')

# percent of children achieving permanency

sp_perm <- stored_procedure('ooh_outcomes')
query_perm <- sqlQuery(annie, sp_perm)
clean_perm <- cr_clean(query_perm, date = F)

length_stay <- 36

three_year_reun <- filter(clean_perm, months.since.entering.out.of.home.care == length_stay, cd.discharge.type == 1) %>%
    filter(cohort.period == max(cohort.period))

perm_dat <- paste0(round(three_year_reun$percent), '%')
perm_label <- paste('children who reunify with their parents within', length_stay/12, 'years')

perm <- data.frame(perm_dat, perm_label)
names(perm) <- name

odbcCloseAll()

# putting the data together

library(jsonlite)

prettify(toJSON(rbind(investigations, count, outcomes, perm)))

banner = rbind(investigations, count, outcomes, perm)
ia_race = data.frame(`Race/Ethnicity` = c("Native", "White"), `Rate per 1,000` = c(237, 57), check.names = F)
prettify(toJSON(list(banner = banner, ia_race = ia_race)))

