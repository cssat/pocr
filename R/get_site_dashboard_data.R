#' Get dashboard data.
#'
#' @description This is a function to update the data for the website dashboards that
#' should be run after each data load.
#'
#' @param con A ODBC connection
#'
#' @return The function returns a json object.
#'
#' @import stringr
#' @import jsonlite
#'
#' @export

get_site_dashboard_data <- function(con) {

    name <- c('label', 'raw_data', 'formatted_data')

    # function to shorten OOH labels

    shorten_ooh_label <- function(x) {
        str_replace_all(x, 'Still in Out-of-Home Care', 'Still in Care')
    }

    # DASHBOARD 1 TAB
    # opened iunvestigations comes from most recent count of invesitations and assessments

    sp_investigations <- stored_procedure('ia_trends_counts')
    query_investigations <- sqlQuery(con, sp_investigations)
    clean_investigations <- cr_clean(query_investigations, date.type = 2)

    investigations <- filter(clean_investigations, date == max(date)) %>%
        mutate(formatted = opened.investigations.and.assessments)

    names(investigations) <- name

    investigations$formatted_data <- format(investigations$formatted_data, big.mark = ',')

    investigations$label <- "investigations into reported child abuse or neglect"

    # INVESTIGATIONS DATA
    # RACE/ETHNICITY

    sp_invest_raceeth <- stored_procedure('ia_trends_rates', ethnicity = c(1, 3, 5, 8, 9))
    query_invest_raceeth <- sqlQuery(con, sp_invest_raceeth)
    clean_invest_raceeth <- cr_clean(query_invest_raceeth, select = 'ethnicity_cd', date.type = 2)

    filt_invest_raceeth <- filter(clean_invest_raceeth, date == max(date)) %>%
        mutate(raw_data = round(opened.investigations.and.assessments),
               formatted_data = round(opened.investigations.and.assessments)) %>%
        select(race.ethnicity, raw_data, formatted_data) %>%
        arrange(-formatted_data)

    names(filt_invest_raceeth) <- name

    # REPORTERS

    sp_invest_report <- stored_procedure('ia_trends_counts', reporter = c(1:14))
    query_invest_report <- sqlQuery(con, sp_invest_report)
    clean_invest_report <- cr_clean(query_invest_report , select = 'cd_reporter_type', date.type = 2)

    filt_invest_report <- filter(clean_invest_report, date == max(date)) %>%
        mutate(count = sum(opened.investigations.and.assessments)) %>%
        group_by(reporter.desc) %>%
        summarize(raw_data = round((opened.investigations.and.assessments/count) * 100)) %>%
        mutate(formatted_data = paste0(raw_data, '%')) %>%
        filter(reporter.desc == 'Social Service Professional'|
                   reporter.desc == 'Educator'|
                   reporter.desc == 'Parent/Guardian'|
                   reporter.desc == 'Law Enforcement Officer')
    names(filt_invest_report) <- name

    reporter.desc <- 'Other'
    raw_data <- 100 - sum(filt_invest_report$raw_data)
    other_rep <- data.frame(reporter.desc, raw_data, paste0(raw_data, '%'))
    names(other_rep) <- name

    reporters <- rbind(filt_invest_report, other_rep)

    # REASONS

    sp_invest_reason <- stored_procedure('ia_trends_counts', allegation = c(1:3))
    query_invest_reason <- sqlQuery(con, sp_invest_reason)
    clean_invest_reason <- cr_clean(query_invest_reason, select = 'cd_allegation', date.type = 2)

    filt_invest_reason <- filter(clean_invest_reason, date == max(date)) %>%
        mutate(reports = format(opened.investigations.and.assessments, big.mark = ',')) %>%
        select(allegation, opened.investigations.and.assessments, reports) %>%
        arrange(allegation)

    names(filt_invest_reason) <- name

    # data for subtitle

    invest_date <- pretty_date(filter(clean_invest_reason, date == max(date), cd.allegation == 1)$date)

    # putting together for json

    list_invest <- list(title = 'Investigations into reported child abuse and neglect', subtitle = invest_date, meta = 'This includes CPS investigations as well as other assessments and services such as FAR and CFWS.', highlights = investigations)

    list_invest_raceeth <- list(title = 'Who experiences investigations?', type = 'table', meta = '', data = filt_invest_raceeth)
    list_reporters <- list(title = 'Who reported these cases?', type = 'donut', meta = '', data = reporters)
    list_reason <- list(title = 'Why are households investigated?', type = 'bar', meta = 'number of reports', data = filt_invest_reason)
    list_dashboard = list(dashboard = list(raceeth = list_invest_raceeth, reporters = list_reporters, reason = list_reason))

    list_ia <- list(dash1 = c(list_invest, list_dashboard))

    ### DASHBOARD 2 TAB
    # count of children in out of home care

    sp_count_ooh <- stored_procedure('ooh_pit_counts')
    query_count_ooh <- sqlQuery(con, sp_count_ooh)
    clean_count_ooh <- cr_clean(query_count_ooh)

    count <- filter(clean_count_ooh, date == max(date)) %>%
        mutate(formatted_data = total.in.out.of.home.care.1st.day)
    names(count) <- name

    count$formatted_data <- format(count$formatted_data, big.mark = ',')
    count$label <- paste('children in out-of-home care on', as.character(pretty_date(count$label)))

    # OOH
    # RACE/ETHNICITY

    sp_ooh_raceeth <- stored_procedure('ooh_pit_rates', ethnicity = c(1, 3, 5, 8, 9))
    query_ooh_raceeth <- sqlQuery(con, sp_ooh_raceeth)
    clean_ooh_raceeth <- cr_clean(query_ooh_raceeth, select = 'ethnicity_cd', date.type = 2)

    ooh_raceeth <- filter(clean_ooh_raceeth, date == max(date)) %>%
        mutate(raw_data = round(total.in.out.of.home.care.1st.day),
               formatted_data = round(total.in.out.of.home.care.1st.day, 1)) %>%
        select(race.ethnicity, raw_data, formatted_data) %>%
        arrange(-formatted_data)

    names(ooh_raceeth) <- name

    # AGE IN CARE

    sp_ooh_age <- stored_procedure('ooh_pit_count', age = c(1:4))
    query_ooh_age <- sqlQuery(con, sp_ooh_age)
    clean_ooh_age <- cr_clean(query_ooh_age , select = 'age_grouping_cd', date.type = 2)

    ooh_age <- filter(clean_ooh_age, date == max(date)) %>%
        mutate(count = sum(total.in.out.of.home.care.1st.day)) %>%
        group_by(age.grouping.cd, age.grouping) %>%
        summarize(raw_data = round((total.in.out.of.home.care.1st.day/count) * 100),
                  formatted_data = paste0(round((total.in.out.of.home.care.1st.day/count) * 100), '%')) %>%
        ungroup() %>%
        select(-age.grouping.cd)

    names(ooh_age) <- name

    ooh_age$label <- stringr::str_trim(str_replace_all(str_replace_all(ooh_age$label, ' through ', '-'), '[a-z(.*)]|[A-Z(.*)]|\\(|\\)', ''))

    ooh_age$label <- paste(ooh_age$label, 'Years Old')

    # COUNTY HIGHLIGHTS

    # getting the 4 counties with the most kids in ooh
    sp_ooh_county_count <- stored_procedure('ooh_pit_count', county = c(1:34))
    query_ooh_county_count <- sqlQuery(con, sp_ooh_county_count)
    clean_ooh_county_count <- cr_clean(query_ooh_county_count, select = 'county_cd', date.type = 2)

    high_cnt_county <- filter(clean_ooh_county_count, date == max(date)) %>%
        arrange(-total.in.out.of.home.care.1st.day) %>%
        slice(1:4) %>%
        select(county.cd)

    sp_ooh_county <- stored_procedure('ooh_pit_rates', county = high_cnt_county[, 1])
    query_ooh_county <- sqlQuery(con, sp_ooh_county)
    clean_ooh_county <- cr_clean(query_ooh_county, select = 'county_cd', date.type = 2)

    ooh_county <- filter(clean_ooh_county, date == max(date)) %>%
        mutate(raw_data = round(total.in.out.of.home.care.1st.day),
               formatted_data = round(total.in.out.of.home.care.1st.day, 1)) %>%
        arrange(-formatted_data) %>%
        select(county, raw_data, formatted_data)

    names(ooh_county) <- name

    # getting data for subtitle

    ooh_subtitle <- pretty_date(filter(clean_ooh_raceeth, date == max(date), ethnicity.cd == 1)$date)

    # putting the data together

    list_hl_ooh <- list(title = 'Children in Out-of-Home Care', subtitle = ooh_subtitle, meta = '', highlights = count)

    list_ooh_raceeth <- list(title = 'Who is in out-of-home care?', type = 'table', meta = '', data = ooh_raceeth)
    list_ooh_age <- list(title = 'How old are children in care', type = 'donut', meta = '', data = ooh_age)
    list_ooh_county <- list(title = "How do Washington's counties compare?", type = 'bar', meta = 'rate of children in care per 1,000', data = ooh_county)
    list_dashboard = list(dashboard = list(raceeth = list_ooh_raceeth, age = list_ooh_age, county = list_ooh_county))

    list_ooh <- list(dash2 = c(list_hl_ooh, list_dashboard))

    ### DASHBOARD 3 TAB
    # outcomes

    sp_outcomes <- stored_procedure('ooh_outcomes')
    query_outcomes <- sqlQuery(con, sp_outcomes)
    clean_outcomes <- cr_clean(query_outcomes, date = F)

    outcomes_date <- filter(clean_outcomes, percent < 50, cd.discharge.type == 0) %>%
        filter(cohort.period == max(cohort.period)) %>%
        select(cohort.period)

    min_dat <- filter(
        clean_outcomes, cohort.period == as.numeric(outcomes_date[1, ]),
        cd.discharge.type == 0, percent <= 50
    ) %>%
        filter(percent == max(percent))

    max_dat <- filter(clean_outcomes, cohort.period == as.numeric(outcomes_date[1, ]), cd.discharge.type == 0, percent >= 50) %>%
        filter(percent == min(percent))

    outcomes_dat <- round(approx(x = c(min_dat$percent, max_dat$percent), y = c(min_dat$months.since.entering.out.of.home.care, max_dat$months.since.entering.out.of.home.care), xout = 50)$y)
    format_dat <- paste(outcomes_dat, 'Months')

    outcomes_label <- "median length of stay in out-of-home care"

    outcomes <- data.frame(outcomes_label, outcomes_dat, format_dat)
    names(outcomes) <- name

    ### Highlights

    recent_date <- filter(clean_outcomes, months.since.entering.out.of.home.care == 24, cd.discharge.type == 0) %>%
        filter(cohort.period == max(cohort.period)) %>%
        select(cohort.period)

    recent_data <- filter(clean_outcomes, cohort.period == recent_date[1,], cd.discharge.type != 6) %>%
        filter(cd.discharge.type == 0 | cd.discharge.type == 1 | cd.discharge.type == 3)
    recent_data$discharge <- str_replace(recent_data$discharge, 'Out-of-Home ', '')

    # 6 MONTHS

    six_months <- filter(recent_data, months.since.entering.out.of.home.care == 6, cd.discharge.type == 0) %>%
        mutate(label = 'Less Than 6 Months',
               raw_data = round(100-percent),
               formatted_data = paste0(round(100 - percent), '%')) %>%
        select(label, raw_data, formatted_data)

    names(six_months) <- name

    # 1 YEAR

    one_year <- filter(recent_data, months.since.entering.out.of.home.care == 12, cd.discharge.type == 0) %>%
        mutate(label = 'Less Than 1 Year',
               raw_data = round(100 - percent),
               formatted_data = paste0(round(100 - percent), '%')) %>%
        select(label, raw_data, formatted_data)

    names(one_year) <- name

    # 2 YEARS

    two_years <- filter(recent_data, months.since.entering.out.of.home.care == 24, cd.discharge.type == 0) %>%
        mutate(label = 'Less Than 2 Years',
               raw_data = round(100 - percent),
               formatted_data = paste0(round(100 - percent), '%')) %>%
        select(label, raw_data, formatted_data)

    names(two_years) <- name

    # highlights

    list_hl_los <- list(title = 'How long do children stay in Care?', subtitle = outcomes_date[1, ], meta = '', highlights = outcomes)

    list_6_months <- list(title = 'Less Than 6 Months', type = 'stat', meta = '', data = six_months)
    list_1_year <- list(title = 'Less Than 1 Year', type = 'stat', meta = '', data = one_year)
    list_2_year <- list(title = 'Less Than 2 Years', type = 'stat', meta = '', data = two_years)

    list_dashboard <- list(dashboard = list('six-months' = list_6_months, 'one-year' = list_1_year, 'two-years' = list_2_year))

    list_los <- list(dash3 = c(list_hl_los, list_dashboard))

    ### OUTCOMES
    # percent of children achieving permanency

    sp_perm <- stored_procedure('ooh_outcomes')
    query_perm <- sqlQuery(con, sp_perm)
    clean_perm <- cr_clean(query_perm, date = F)

    length_stay <- 36

    perm <- filter(clean_perm, months.since.entering.out.of.home.care == length_stay, cd.discharge.type == 1) %>%
        filter(cohort.period == max(cohort.period)) %>%
        mutate(raw_data = round(percent),
               formatted_data = paste0(round(percent), '%')) %>%
        select(discharge, raw_data, formatted_data)

    names(perm) <- name

    perm$label <- paste('children who reunify with their parents within', length_stay/12, 'years')

    ### HIGHLIGHTS

    sp_perm_hl <- stored_procedure('ooh_outcomes', age = c(0:8))
    query_perm_hl <- sqlQuery(con, sp_perm_hl)
    clean_perm_hl <- cr_clean(query_perm_hl, date = F, select = 'age_grouping_cd')

    clean_perm_hl <- filter(clean_perm_hl, cd.discharge.type == 0 | cd.discharge.type == 1 | cd.discharge.type == 3,
                            age.grouping.cd == 0 | age.grouping.cd == 1 | age.grouping.cd == 4 | age.grouping.cd == 6,
                            months.since.entering.out.of.home.care == length_stay) %>%
        filter(cohort.period == max(cohort.period))

    clean_perm_hl$age.grouping <- ifelse(str_detect(clean_perm_hl$age.grouping, 'All'), 'All Ages',
                                         ifelse(str_detect(clean_perm_hl$age.grouping, 'Infancy'), 'Less Than 1',
                                                ifelse(str_detect(clean_perm_hl$age.grouping, 'Pre-School'), '3-4', '10-14')))

    clean_perm_hl$age.grouping <- ifelse(str_detect(clean_perm_hl$age.grouping, '3-4|10-14'), paste(clean_perm_hl$age.grouping, 'Years Old'), clean_perm_hl$age.grouping)

    clean_perm_hl$age.grouping <- stringr::str_trim(clean_perm_hl$age.grouping)

    # data by topic and age group

    still_ooh <- filter(clean_perm_hl, cd.discharge.type == 0) %>%
        mutate(raw_data = round(percent),
               formatted_data = paste0(round(percent), '%')) %>%
        select(age.grouping, raw_data, formatted_data)

    names(still_ooh) <- name

    reunification <- filter(clean_perm_hl, cd.discharge.type == 1) %>%
        mutate(raw_data = round(percent),
               formatted_data = paste0(round(percent), '%')) %>%
        select(age.grouping, raw_data, formatted_data)

    names(reunification) <- name

    adoption <- filter(clean_perm_hl, cd.discharge.type == 3) %>%
        mutate(raw_data = round(percent),
               formatted_data = paste0(round(percent), '%')) %>%
        select(age.grouping, raw_data, formatted_data)

    names(adoption) <- name

    # getting data for subtitle

    subtitle_data <- filter(clean_perm_hl, cohort.period == max(cohort.period), age.grouping.cd == 0, cd.discharge.type == 0)$cohort.period

    # putting the data together

    list_hl_outcomes <- list(title = 'Outcomes within 3 Years', subtitle = subtitle_data[1], meta = '', highlights = perm)

    list_still_ooh <- list(title = 'Still in Care', type = 'bar', meta = '', data = still_ooh)
    list_reunification <- list(title = 'Reunification', type = 'bar', meta = '', data = reunification)
    list_adoption <- list(title = 'Adoption', type = 'bar', meta = '', data = adoption)

    list_dashboard <- list(dashboard = list('still-in-ooh' = list_still_ooh, reunification = list_reunification, adoption = list_adoption))

    list_outcomes <- list(dash4 = c(list_hl_outcomes, list_dashboard))

    # creating the json

    dashboard_json <- toJSON(list(list_ia, list_ooh, list_los, list_outcomes), auto_unbox = TRUE, pretty = TRUE)

    return(dashboard_json)
}

