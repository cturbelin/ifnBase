# ifnBase 0.4.0.9000

* `recode_intake` now recode all available variables
* remove function `coalesce()`
* fix season censoring using `get_season_censoring()` in platform env
* censoring can be parametrized passing list() in `first.season` in `load_incidence_results()`
* episode function now handle fusion strategies and end date of episode computation
* 'ari*' definitions provided by `SyndromeProviderRS2019` dont anymore consider pain and headache for child under age defined by `pain.age.limit`
* `recode_weekly` check if data was already recoded and dont recode it, added flags
* added constants objects in uppercase YES, NO, DONTKNOW, see `?constants`
* added concepts page `?concepts`

# ifnBase 0.4.0

* introduce episodes computation functions
* extract onset computation from incidence `IncidenceRS2014` & `IncidenceDailyRS2014` now done in `load_incidence_results`, remove `onset.columns` parameter, date recoding are extracted (done in `recode_weekly`).
* onset is computed using a quosure by `compute_onset()`
* add `episode_onset_design()` returning expression to compute onset date used for episodes
* add `base_onset_design()`  returning expression to compute onset date used for incidence
* `load_incidence_results()` get a parameter 'onset' to get the onset expression, returns now a class "incidence_loader"
* date recoding are in `recode_weekly_date`
* `recode_weekly()` now recodes dates using `recode_weekly_date` adds an attribute "recode_weekly" to flag the weekly as recoded 

# ifnBase 0.3.2

* Fix namespace imports
* Fix function name

# ifnBase 0.3.1
* add `compute_weekly_syndromes` to create syndrome columns in weekly using several kind of strategies
* add `complete_intake_strategy()` and `complete_intake()` for intake survey completion accross seasons
* add `load_results_for_incidence` to load & prepare data in order to compute incidence
* add `SyndromeProviderCountry` R6 class to compute by country definition (draft)
* add `SyndromeProviderClass` as base R6 class for syndrome provider classes

# ifnBase 0.3
* `SyndromeProviderRS2019` R6 class compute 2019 revised syndrom definition
* adding incidence functions as `R6` classes `IncidenceRS2014` and `IncidenceDailyRS2014`
* fix `plot_age_pyramid()` scale
* add `theme_with()` for ggplot receipes
* `syndromes_rs_2018()` embeds all function related to syndromes definitions for Guerrisi,Euro Surv., 2018 
* stats function `multitple_freq()`, `multiple_xfreq()`
* new function `get_db_handle()` to get the db connexion
* rename `cut.age()` function `cut_age()`
* add country in `platform_geographic_levels()`

# ifnBase 0.2

* adding swMisc package as Import dependency https://github.com/sentiweb/swMisc
* Move `merge_list()`, `ending_slash()`, `get_r_file()` and misc functions (not used directly in the package) to swMisc 
* rename merge.list() to `merge_list()`, `keep.last.survey()` to `keep_last_survey()`, `calc.age()` to `calc_age()`
* recode_weekly: weekly date is computed using truncated POSIXct value to day
* Added a `NEWS.md` file to track changes to the package.
* adding ggplot2 related functions, ggplot is now an Import dependency
* ISOYearWeek() renamed to `iso_yearweek()`
* `makeWeekIndex()` renamed to `make_week_index()`
* `mondayOfDate()` renamed to `monday_of_date()`
* `WeekStart()` renamed to `monday_of_week()`
* `YearStart()` renamed to `monday_of_year()`
* `weesbydates()` renamed to `weeks_of_date()`
* `WeekStamp()` renamed to `week_stamp()`
* fixing documentation
