# Init/Libraries
for (pkg in c('RODBC','keyring','dplyr','ggplot2','here','lubridate','uwdem.pkg','readxl')) {
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
odbc_name <- 'R_Clarity'
oracle_conn <- odbcConnect(
	odbc_name,
	uid=keyring::key_list(odbc_name)$username,
	pwd=keyring::key_get(odbc_name,keyring::key_list(odbc_name)$username))


q <- paste0("
select
     f_enc.adt_arrival_date
    ,f_enc.pat_enc_csn_id
    ,disp.department_id
    ,f_enc.age_at_arrival_years as age
from f_ed_encounters f_enc
inner join v_uwh_ed_disp_info disp on disp.pat_enc_csn_id = f_enc.pat_enc_csn_id
where
    disp.visit_inclusion_grouper = 1
    and disp.department_id in (22122,37001)
    and f_enc.adt_arrival_date >= to_date('2021-01-01','yyyy-mm-dd')
")

patients <- sqlQuery(oracle_conn,q)
odbcCloseAll()

path_to_file <- file.path(here::here('Informed About Delays'),'pg_informed_about_delays.xlsx')
col_types <- c(
	'text'      #CYMO
	,'text'     #Location
	,'text'     #Question Text
	,'numeric'  #N              
	,'numeric'  #Top Box

)
df <- readxl::read_xlsx(path_to_file,sheet = 1, col_types = col_types)
head(df)
names(df)

vol_rating_df <- inner_join(
	patients %>%
		mutate(Location = factor(DEPARTMENT_ID, levels = c(22122,37001), labels = c('UH','EMH'))) %>%
		group_by(Location,CYMO = as.character(floor_date(ADT_ARRIVAL_DATE,'month'), format = 'CY%Y-%m')) %>%
		summarise(PATS = n())
	,df %>%
		mutate(Location = factor(Location, levels = c('UH','TAC'), labels = c('UH','EMH'))) %>%
		rename(top_box = `Top Box`) %>%
		select(CYMO,Location, top_box)
	,by = c('CYMO','Location')
)
vol_rating_df


vol_rating_df %>%
	mutate(CYMO = ymd(gsub('^CY','',CYMO) %+% '-01')) %>%
	ggplot(aes(x=CYMO,y=top_box, color = Location)) +
	geom_point(aes(size=PATS)) +
	stat_smooth() +
	scale_y_continuous(name = 'Top Box Rating for Informed About Wait Times', limits = c(0,1), labels = scales::percent) +
	scale_x_date(name = 'Month', breaks = seq(as.Date('2021-01-01'),as.Date('2022-06-01'),by = 'month'), labels = function(x) {as.character(x,format='%b %Y')}) +
	scale_color_manual(values = get_uwdem_colors()) +
	uwdemdann_format(
		Title = 'Top Box Rating for Informed About Wait Times and Monthly Patient Volume',
		Source = 'Press Ganey and Clarity'
	)



cor(subset(vol_rating_df,Location == 'UH')$PATS,subset(vol_rating_df,Location == 'UH')$top_box)


by(vol_rating_df,vol_rating_df$Location,FUN = cor, x = vol_rating_df$PATS, y = vol_rating_df$top_box)


vol_rating_df %>%
	group_by(Location) %>%
	summarise(COR = cor(PATS,top_box))
