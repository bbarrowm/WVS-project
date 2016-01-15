##Untitled Wolchik CEE WVS study

##outstanding issues
##Brazil observations aren't present for 1994-1998 wave, although WVS indicates it was included
##Phillipines not in survey dataset, although appears to be in separate wave 3 dataset
##Determine how to deal with future waves. Should we stick to same countries, or apply
##criteria to unique countries in each wave?



##replicate "the weakness of civil society"

##dependent variable: organizational membership index
##individual level: index of total number of claimed membership
##in 9 voluntary orgs (active or inactive). Min = 0, Max = 9
##country level: avg number of organizational memberships per person for whole country

##Country-level IVs
##GDP per capita: 1998 CIA WF, in thousands
##Political Rights and Civil Liberties: FH 1995-96 and 96=97, scale inverted
##min = 1, max = 7 (more liberties)
##Civilization: from Huntington, 0= non-Western, 1 = Western
##Prior regime type: two dummy variables (from Linz Totalitarian and Auth. Regimes)
##prior authoritarian experience: 0 = no, 1 = yes
##prior communist experience: 0 = no, 1 = yes





##step 1: country level analysis

##does organizational membership vary by prior regime type?
##exclude countries Armenia, Colombia, Dominican Rep, Ghana, Great Britain,
##Moldova, Poland, Taiwan (analyze missing values to see why?)
##include only countries at avg. 3.5 or better on Freedom House scores for both years
##criterion excludes Azerbaijan, Belarus, BiH, China, Croatia, Georgia, India,
##Mexico, Nigeria, Pakistan, Peru, Turkey, Yugoslavia
##study includes countries in 3 groups:
##older democracies: Australia, Finland, Japan, Norway, Sweden, Switzerland, USA,
##West Germany
##post-Authoritarian: Argentina, Bangladesh, Brazil, Chile, Phillipines, South Africa
##South Korea, Spain, Uruguay, Venezuela
##post-communist: Bulgaria, Czech, Eastern Germany, Estonia, Hungary, Latvia,
##Lithuania, Macedonia, Romania, Russia, Slovakia, Slovenia, Ukraine

##load 1981-2014 integrated dataset
library(dplyr)
library(foreign)
library(car)
library(lattice)
library(ggplot2)


##read-in using stata format
##converts codes to stata value labels
##converts date-times to 'POSIXct class' and Stata dates to R 'date' class
wvs <-
  read.dta(
    "WVS_Longitudinal_1981_2014_stata_v2015_04_18.dta", convert.dates = T,
    convert.factors = T
  )

##subset wvs data frame to variables of interest:
##dependent variable: organizational membership
##individual level: index of total number of claimed membership (A098:A106;
##A106B and A106C asked in 6th wave only) in 9 voluntary orgs (active or inactive).
##Min = 0, Max = 9
##robustness check (not in howard): belong to x (A064:A080_f)
##country level: avg number of organizational memberships per person for whole country

##Individual level IVs:
##Income: 1-10 scale (country specific?): (X047: X047R)
##Education: 1-9 scale (X025: X025R) (country specific codes: X025CS)
##Age: coded in yeears (X002, X003:X003R2)
##Gender: 0 = female, 1 = male (X001)
##City size: 1-8 scale (1 = under 2000, 8 = 500k or more) (X049:X049CS)
##Interpersonal trust: 0 = can't be too careful, 1 = most people can be trusted (A165)
##TV watching: 1-4 scale (1 = don't watch TV, 4 = 3+ hpd)(E188)
##Postmaterialism (from Inglehart) batteres: (Y001:Y002)

##meta variables
##wave (S002)
##country (S003), country with split ups (S003A)
##respondent identifiers (S006: s007_01)
##weights (S017:S019A)
##survey year (S020)
##country-wave (S024)
##country-year (S025)

##subset data and rename variables
WVS <-
  select(
    wvs, wave = S002, country = S003, country_split = S003A,
    original_id = S006, unified_id = S007, evs_integrated_id = S007_01, weight = S017,
    weight_splits = S017A, eq_weight_1000 = S018, eq_weight_1000_splits = S018A,
    eq_weight_1500 = S019, eq_weight_1500_splits = S019A, survey_year = S020,
    country_wave = S024, country_year = S025, female = X001, birth_year = X002, age = X003,
    age_6interval = X003R, age_3interval = X003R2, education = X025, ISCED_1 = X025A,
    ISCED_2 = X025B, ISCED_3 = X025C, education_cs = X025CS, education_3_level = X025R,
    income_scale = X047, weekly_hh_income = X047A, weekly_hh_income_cs = X047A_01,
    monthly_hh_income = X047B, monthly_hh_income_cs = X047B_01, annual_hh_income = X047C,
    annual_hh_income_cs = X047C_01, income_cs = X047CS, monthly_hh_income_ppp = X047D,
    income_3level = X047R, town_size = X049, town_size_cs = X049CS,belong_elderly = A064,
    belong_religious = A065, belong_edu = A066,
    belong_union = A067, belong_political_party = A068, belong_local_political = A069,
    belong_human_rights = A070, belong_conservation_animal = A071,
    belong_conservation = A071B, belong_animal = A071C, belong_professional = A072,
    belong_youth = A073, belong_sports = A074, belong_womens = A075, belong_peace = A076,
    belong_health = A077, belong_consumer = A078, belong_other = A079, belong_none = A080,
    belong_none_flag = A080_F, church = A098, sports = A099, art_music_ed = A100, union = A101,
    political_party = A102, environmental = A103, professional = A104,
    humanitarian = A105, other_org = A106, consumer = A106B, self_help = A106C,
    interpersonal_trust = A165, tv = E188, post_materialist_12 = Y001,
    post_materialist_4 = Y002
  )
##remove big dataset
rm("wvs")
##change to tbl_df class for easier viewing
WVS <- tbl_df(WVS)
##examine subset
WVS
str(WVS)
head(WVS)
tail(WVS)

##create participation index
##WVS contains a battery of questions asking respondents to indicate whether they are
##members of a series of organizations (church, sports, art/music/ed, labor union,
##political party, environmental, professional, humanitarian, other)
##default wording: active or inactive member of X organization
##default coding: 0 = not a member; 1 = inactive member, 2 = active member
##defalut coding: values < 0 indicate NA (missing, unknown, not asked in survey, no answer, don't know)
##This analysis uses the factor values instead of numerical codes
##final index is a variable ranging from 0 to 9, indicating the total number
##of organizaitons in which a respondent is an inactive or active member
##per methodology of Howard (2003)

##creates subset with 9 participation variables
temp <- select(WVS, church:other_org)
##creates index with score of total active or inactive memberships in 9 orgs
##for each individual
part_index <-
  rowSums(temp == "Active member" | temp == "Inactive member")
##view new index variable
part_index
head(part_index) ##view index scores for first five observations
tail(part_index) ##last five scores
##summary statistics for index variable
summary(part_index)

##insert new index variable into larger WVS dataset
WVS <- cbind(WVS, part_index)
##remove temp data frame
rm("temp")
##remove independent index object
rm("part_index")
WVS <- tbl_df(WVS)


##Start Howard (2003) Analysis

##subset to wave and countries
##countries:
##older democracies: Australia, Finland, Japan, Norway, Sweden, Switzerland, USA,
##West Germany
##post-Authoritarian: Argentina, Bangladesh, Brazil, Chile, Philippines, South Africa
##South Korea, Spain, Uruguay, Venezuela
##post-communist: Bulgaria, Czech, Eastern Germany, Estonia, Hungary, Latvia,
##Lithuania, Macedonia, Romania, Russia, Slovakia, Slovenia, Ukraine

summary(WVS$country_split)
summary(WVS$wave)
howard <-
  WVS %>%
  filter (wave == "1994-1998") %>%
  filter (
    country_split == "Australia" |
      country_split == "Finland" | country_split == "Japan" |
      country_split == "Norway" | country_split == "Sweden" |
      country_split == "Switzerland" |
      country_split == "United States" |
      country_split == "Germany" |
      country_split == "West Germany" |
      country_split == "Argentina" | country_split == "Bangladesh" |
      country_split == "Brazil" |  country_split == "Chile" |
      country_split == "Philippines" |
      country_split == "South Africa" |
      country_split == "South Korea" | country_split == "Spain" |
      country_split == "Uruguay" |  country_split == "Venezuela" |
      country_split == "Bulgaria" | country_split == "Czech Rep." |
      country_split == "East Germany" |
      country_split == "Estonia" |
      country_split == "Hungary" | country_split == "Latvia" |
      country_split == "Lithuania" | country_split == "Macedonia" |
      country_split == "Romania" |  country_split == "Russia" |
      country_split == "Slovakia" | country_split == "Slovenia" |
      country_split == "Ukraine"
  )

##generate average participation scores by country
part_by_country <-
  howard %>% group_by(country_split) %>% summarize(mean(part_index))
data.frame(part_by_country)

##create regime type variable
temp <-
  recode(
    howard$country_split, "c('Australia', 'Finland', 'Japan', 'Norway',
    'Sweden', 'Switzerland', 'United States', 'West Germany') = 'Older Democracies';
    c('Argentina', 'Bangladesh', 'Brazil', 'Chile', 'Philippines', 'South Africa',
    'South Korea', 'Spain', 'Uruguay', 'Venezuela') = 'Post-authoritarian';
    c('Bulgaria', 'Czech Rep.', 'Estonia', 'Hungary', 'Latvia',
    'Lithuania', 'Macedonia', 'Romania', 'Russia', 'Slovakia', 'Slovenia',
    'Ukraine', 'East Germany') = 'Post-communist'"
    )
summary(temp)

##check recode
sum(temp == "Older Democracies") == sum(
  howard$country_split == "Australia" |
    howard$country_split == "Finland" |
    howard$country_split == "Japan" |
    howard$country_split == "Norway" |
    howard$country_split == "Sweden" |
    howard$country_split == "Switzerland" |
    howard$country_split == "United States" |
    howard$country_split == "West Germany"
)
sum(temp == "Post-authoritarian") == sum(
  howard$country_split == "Argentina" |
    howard$country_split == "Bangladesh" |
    howard$country_split == "Brazil" |
    howard$country_split == "Chile" |
    howard$country_split == "Philippines" |
    howard$country_split == "South Africa" |
    howard$country_split == "South Korea" |
    howard$country_split == "Spain" |
    howard$country_split == "Uruguay" |
    howard$country_split == "Venezuela"
)
sum(temp == "Post-communist") == sum(
  howard$country_split == "Bulgaria" |
    howard$country_split == "Czech Rep." |
    howard$country_split == "Estonia" |
    howard$country_split == "Hungary" |
    howard$country_split == "Latvia" |
    howard$country_split == "Lithuania" |
    howard$country_split == "Macedonia" |
    howard$country_split == "Romania" |
    howard$country_split == "Russia" |
    howard$country_split == "Slovakia" |
    howard$country_split == "Slovenia" |
    howard$country_split == "Ukraine" |
    howard$country_split == "East Germany"
)

##insert regime type variable into 'howard' subset
howard$regime <- temp
##remove temporary regime variable
rm("temp")

##specific organizational membership by country
##percentage of respondents in each country that are members of given organization

org_membership_country <-
  howard %>%
  group_by(country_split) %>%
  summarise(
    count = n(),
    church_perc = ((
      sum(church == "Active member" |
            church == "Inactive member")
    ) / count) * 100,
    sports_perc = ((
      sum(sports == "Active member" |
            sports == "Inactive member")
    ) / count) * 100,
    art_music_ed_perc = ((
      sum(
        art_music_ed == "Active member" |
          art_music_ed == "Inactive member"
      )
    ) / count) * 100,
    union_perc = ((
      sum(union == "Active member" |
            union == "Inactive member")
    ) / count) * 100,
    political_party_perc = ((
      sum(
        political_party == "Active member" |
          political_party == "Inactive member"
      )
    ) / count) * 100,
    environmental_perc = ((
      sum(
        environmental == "Active member" |
          environmental == "Inactive member"
      )
    ) / count) * 100,
    professional_perc = ((
      sum(
        professional == "Active member" |
          professional == "Inactive member"
      )
    ) / count) * 100,
    humanitarian_perc = ((
      sum(
        humanitarian == "Active member" |
          humanitarian == "Inactive member"
      )
    ) / count) * 100,
    other_perc = ((
      sum(other_org == "Active member" |
            other_org == "Inactive member")
    ) / count) * 100
  )

data.frame(org_membership_country)

##specific organizational membership by regime type
##percentage of respondents within each regime type that are members of given org

org_membership_regime <-
  howard %>%
  group_by(regime) %>%
  summarise(
    count = n(),
    church_perc = ((
      sum(church == "Active member" |
            church == "Inactive member")
    ) / count) * 100,
    sports_perc = ((
      sum(sports == "Active member" |
            sports == "Inactive member")
    ) / count) * 100,
    art_music_ed_perc = ((
      sum(
        art_music_ed == "Active member" |
          art_music_ed == "Inactive member"
      )
    ) / count) * 100,
    union_perc = ((
      sum(union == "Active member" |
            union == "Inactive member")
    ) / count) * 100,
    political_party_perc = ((
      sum(
        political_party == "Active member" |
          political_party == "Inactive member"
      )
    ) / count) * 100,
    environmental_perc = ((
      sum(
        environmental == "Active member" |
          environmental == "Inactive member"
      )
    ) / count) * 100,
    professional_perc = ((
      sum(
        professional == "Active member" |
          professional == "Inactive member"
      )
    ) / count) * 100,
    humanitarian_perc = ((
      sum(
        humanitarian == "Active member" |
          humanitarian == "Inactive member"
      )
    ) / count) * 100,
    other_perc = ((
      sum(other_org == "Active member" |
            other_org == "Inactive member")
    ) / count) * 100
  )

data.frame(org_membership_regime)




##generate average participation by regime type
##table pp. 167: Macedonia & Slovenia excluded from PC group; Japan from OD group;
##Bangladesh from PA group
##Phillipines and Brazil missing from WVS dataset
part_by_regime <-
  howard %>% group_by(regime) %>% summarize(mean(part_index))
part_by_regime





##pp 62, figure 4.1
##plot bar chart: y axis = no. org memberships per person, x axis = regime type groups


barplot(part_by_regime, col = "blue")



##pp 64, figure 4.2
##bar chart: y=%respondents members, x=specific organziations by prior regime type]

##pp 65-66, table 4.2
##organizational membership by country (by percent)

##pp 69, figure 4.3
##number of total organizational memberships per person by country and prior regime type

##pp 71, table 4.3
##changes in country rankings of levels of membership from 91 wave to 96 wave
##rethink. should be able to a time series plot

##boxplot average participation by country
