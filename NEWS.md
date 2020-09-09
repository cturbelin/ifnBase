# ifnBase 0.4.10 

* `SyndromeProviderRS2019` rename 'ili' to 'ili.rs' and revert fever level, 'ari.ecdc' will now 'ili.ecdc'

# ifnBase 0.4.9 

* `SyndromeProviderRS2019` fix fever level for 'ili' syndrome
* `episode_fusion_strategy` fix parameter episode.column

# ifnBase 0.4.8 

* `design_incidence()` now accept `use.gender` argument
* `calc_adjusted_incidence()` handles `use.gender` in strata design

# infBase 0.4.7

* `IncidenceRS2014` can now extract active participants for each week (with 'participant' in output)
* add `i18n_set()` tod define translation directly
* add `NO_SYMPTOM` constant for question weekly::Q1
* add geographic code normalizer (mishandled by some db provider)
* add `create_path()`function to create path without changing the current
* `survey_variable_available()`handles *country* parameter
* *RSQLite* as suggested package (not tested yet)
* *sympt.cause* is now loaded for incidence (needed to compute syndromic ifn)
* add `SyndromeProviderIfn` class
* `SyndromeProviderRS2019` now expect parameters in constructor instead of `compute()`
* fix `graph_colors()` to handle color brewer correctly, add rainbow failback and *dark* parameter

# ifnBase 0.4.6

* `health.status` options was not handled correctly
* `calc_age()` fixed to correctly handle month
* `load_results_for_incidence()` rename ifn syndrome name if duplicate with a provider
* `SyndromeProviderRS2019` can relax "sudden" criteria

# ifnBase 0.4.5

* `dbConnect()` now accepts port entry in dsn
* new function `survey_rename_columns()` to rename columns from external source (csv)

# ifnBase 0.4.4

# infBase 0.4.3

* add `survey_recode_all()`
* `recode_weekly()` can recode all variables, and check for already recoded highest.temp
* add test for episodes & syndrome

# ifnBase 0.4.2

* add `variable_available()` helper to define by season availability of variables
* `platform_define_survey()` now preserves inherited attributes of variables
* add file based age group population loader & config in platform_options()
* i18n_file now handles both .r and .R files
* add ggplot2 theme_with() helper function
* fix `complete_intake()` if not intake is available
* Refactor `survey_participant_previous_season()` to better handler single table model
* Internal: add db helpers function `db_quote_str()`, `db_quote_var()`, `db_equal()`
* improve documentation, add several vignettes surveys, 
* fix recode_alias()
* add print S3 methods for survey_definition
* add episode fusion strategies
* add functions to manage paths : `get_current_paths()`, `add_path_prefix()`

# ifnBase 0.4.1
* use roxygen 7
* fix recoding name for `main.activity` variable
* recoding are now S3 class `survey_recode` allowing pretty print
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
