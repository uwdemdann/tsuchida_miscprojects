# Init/Libraries
for (pkg in c('RODBC','keyring','dplyr','ggplot2','here','lubridate','uwdem.pkg','openxlsx')) {
    if (!is.element(pkg,installed.packages()[,1])) {
        if (grepl('^uwdem',pkg)) {
            devtools::install_github(paste0('uwdemdann/',pkg))
        } else {
            r <- getOption("repos") ; r["CRAN"] <- "http://cran.us.r-project.org"
            options(repos = r) ; rm(r) ; install.packages(pkg)
        }
    }
    library(pkg,character.only = T, quietly = T, warn.conflicts = F)
} ; rm(pkg)
`%ni%` <- Negate(`%in%`)
`%+%` <- function(lhs,rhs) {paste0(lhs,rhs)}
# Load and Parse Data
path_to_file <- file.path('H:','Tsuchida - Ambassador Baseline.xlsx')
col_types <- c(
     'text'     #PAT_ENC_CSN_ID
    ,'numeric'  #DEPARTMENT_ID
    ,'date'     #ARRIVAL_DTTM
    ,'numeric'  #ARRIVAL_HOUR              
    ,'numeric'  #AGE_AT_ARRIVAL_YEARS
    ,'text'     #REASON_VISIT_NAME
    ,'text'     #ESI
    ,'text'     #DAY_OF_WEEK               
    ,'text'     #WEEKEND_YN
    ,'text'     #PERIOD
    ,'logical'  #ARRIV_WITH_AMBASSADOR
    ,'logical'  #LEFT_LWBS                 
    ,'logical'  #LEFT_LBEC
    ,'logical'  #LEFT_AMA
    ,'logical'  #LEFT_LBTC
    ,'text'     #CARESTART_YN              
    ,'date'     #ARRIVAL
    ,'date'     #ROOMED
    ,'date'     #DEPARTURE
    ,'date'     #PAT_IN_TRANSIT_TAC_TO_UWED
)
df <- readxl::read_xlsx(path_to_file,sheet = 1, col_types = col_types)
head(df)
names(df)

# Summarise by Week

    # Top Level
    df %>% filter(DEPARTMENT_ID == 22122) %>%
        group_by(DEPARTMENT_ID, ARRIV_WITH_AMBASSADOR) %>%
        summarise(
            N = sum(LEFT_LWBS),
            D = n(),
            P = sum(LEFT_LWBS)/n()
        ) %>% 
        mutate(OVERALL_P = sum(N)/sum(D))
    ## N/% by Week for Ambassador time vs not and overall
    daily_df <- df %>% filter(DEPARTMENT_ID == 22122) %>%
        group_by(DEPARTMENT_ID, PERIOD, DATE = as.Date(floor_date(ARRIVAL_DTTM,'day')),ARRIV_WITH_AMBASSADOR = factor(ARRIV_WITH_AMBASSADOR, levels = c(T,F), labels = c('11a-3p','Other Times'))) %>%
        summarise(
            N = sum(LEFT_LWBS),
            D = n(),
            P = sum(LEFT_LWBS)/n()
        ) %>% 
        mutate(OVERALL_P = sum(N)/sum(D), OVERALL_N = sum(N)) %>%
        group_by(DEPARTMENT_ID,ARRIV_WITH_AMBASSADOR) %>%
        mutate(
            seven_day_N = N + lag(N,1,order_by=DATE) + lag(N,2,order_by=DATE) + lag(N,2,order_by=DATE) + lag(N,4,order_by=DATE) + 
                            lag(N,5,order_by=DATE) + lag(N,6,order_by=DATE),
            seven_day_D = D + lag(D,1,order_by=DATE) + lag(D,2,order_by=DATE) + lag(D,2,order_by=DATE) + lag(D,4,order_by=DATE) + 
                            lag(D,5,order_by=DATE) + lag(D,6,order_by=DATE),
            seven_day_P = seven_day_N / seven_day_D
        )

    overall_lwbs <- daily_df %>% 
        group_by(PERIOD) %>% 
        summarise(P = sum(daily_df$N) / sum(daily_df$D))
    daily_df %>%
        ggplot(aes(x=DATE,y=P,color = ARRIV_WITH_AMBASSADOR)) +
            geom_line(aes(alpha = 'Daily')) + geom_point(aes(alpha = 'Daily')) +
            geom_line(aes(y=seven_day_P, alpha = '7-Day Average')) + geom_point(aes(y=seven_day_P, alpha = '7-Day Average')) +
            geom_segment(data = overall_lwbs,aes(color = NULL,y = overall_lwbs[1,]$P, yend= overall_lwbs[1,]$P, x = min(daily_df$DATE), xend = as.Date('2022-07-11') ),linetype = 'dashed', show.legend = F) +
            geom_segment(data = overall_lwbs,aes(color = NULL,y = overall_lwbs[2,]$P, yend= overall_lwbs[2,]$P, x = min(daily_df$DATE), xend = as.Date('2022-07-11') ),linetype = 'dashed', show.legend = F) +
            geom_text(aes(label = 'Overall LWBS %',y = overall_lwbs[1,]$P, x = min(daily_df$DATE)), hjust = 0, vjust = 0, show.legend = F)+
            geom_vline(aes(xintercept = as.Date('2022-07-11')), linetype = 'dashed', color='black', show.legend = F) +
            geom_text(aes(label = 'Start of Ambassador Pilot',y = 0, x = as.Date('2022-07-11')), hjust = 0, vjust = 0, angle = 90, show.legend = F)+
            scale_x_date(name = 'Day', limits = c(min(daily_df$DATE),as.Date('2022-08-30'))) +
            scale_y_continuous(name = 'Daily Percent LWBS',labels = scales::percent) +
            scale_alpha_manual(values = c('Daily'=0.15,'7-Day Average' = 1)) +
            scale_color_manual(values = get_uwdem_colors()) +
            uwdemdann_format(
                Title = 'Daily LWBS During Guest Ambassador Times'
            )


    # Compare EMH vs UH LWBS By Week
    # Compare UH Weekend vs Weekday by Week
    # Compare Ambassador Time vs Not


