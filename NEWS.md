# ifnBase 0.1.4
* adding swMisc package as Import dependency https://github.com/sentiweb/swMisc
* Move merge_list(), ending_slash(), get_r_file() and misc functions (not used directly in the package) to swMisc 
* rename merge.list() to merge_list(), keep.last.survey() to keep_last_survey(), calc.age() to calc_age()
* recode_weekly: weekly date is computed using truncated POSIXct value to day
* Added a `NEWS.md` file to track changes to the package.
* adding ggplot2 related functions, ggplot is now an Import dependency
* ISOYearWeek() renamed to iso_yearweek()
* makeWeekIndex() renamed to make_week_index()
* mondayOfDate() renamed to monday_of_date()
* WeekStart() renamed to monday_of_week()
* YearStart() renamed to monday_of_year()
* weesbydates() renamed to weeks_of_date()
* WeekStamp() renamed to week_stamp()
* fixing documentation
