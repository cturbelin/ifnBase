---
output:
  word_document: default
  html_document: default
  pdf_document: default
---
# InfluenzaNet Incidence Computation

Author: Clément Turbelin <clement.turbelin@sorbonne-unversite.fr>
Version: 1.1, 4 Nov 2019

## Data & Variables used to compute Incidence

### External Data
  @see DataAnalysisGuidelines.md

### Data Loading

Incidence are computed by season, considering the season`s data (from the last september of the season)
Variables will be named in human readable & meaningful variable names, corresponding variable db column name in indicated aside each variable

Intake survey variables:

 - `timestamp`
 - `date.birth` = `Q2`
 - `code_com` = `Q3`

For each participant, the last available intake survey response for the season is considered (only one age and location is considered for a participant during a season)

Across platforms differences:
 
 > For some countries data are stored in the same table and the is no intake for the current season for some participants, 
 > Intake can be loaded from previous seasons but a limit should be used (to be sure very old data are not used) or
 > participants without an intake during the season should be excluded (TBD)
 
 > This is especially the case for IT, ES and UK (2015) counting season from october to april.


Weekly survey variables:

- `timestamp`
- `no.sympt`=`Q1_0`,
- `fever`=`Q1_1`,
- `chills`=`Q1_2`,
- `rhino`=`Q1_3`,
- `sneeze`=`Q1_4`,
- `sorethroat`=`Q1_5`,
- `cough`=`Q1_6`,
- `dyspnea`=`Q1_7`,
- `headache`=`Q1_8`,
- `pain`=`Q1_9`,
- `chestpain`=`Q1_10`,
- `asthenia`=`Q1_11`,
- `anorexia`=`Q1_12`,
- `sputum`=`Q1_13`,
- `wateryeye`=`Q1_14`,
- `nausea`=`Q1_15`,
- `vomiting`=`Q1_16`,
- `diarrhea`=`Q1_17`,
- `abdopain`=`Q1_18`,
- `sympt.other`=`Q1_19`,

- `fever.sudden`=`Q6b`
- `highest.temp`=`Q6d`
- `same.episode`= `Q2`
- `sympt.start`= `Q3_0_open`
- `fever.start`= `Q6_1_open`
- `sympt.sudden`= `Q5`

### Variable Recoding

Recode some variables to make error-proof coding and recode to Missing value inconsistent values (date in the future)

weekly:

- `sympt.sudden` : 0 -> Yes(True), 1 -> No(False)
- `same.episode` : 0 -> "Yes", 1-> "No", 2->"DontKnow", 3->Missing
- `fever.sudden` : 0 -> Yes(True), 1-> No (False)
- `highest.temp` : 6 -> Missing
- `sympt.start` : `sympt.start` > date(`timestamp`) -> Missing
- `fever.start` : `fever.start` > date(`timestamp`) -> Missing

intake:

- age : compute age as difference between yearly date birth and yearly intake timestamp, yearly(date)= year(date) + month(date)/12, age is rounded to 2 decimals

Remarks:

 > inconsistency of date.birth is not checked here, should be (negative and too old people can occur)
 > Inconsistency of age was checked for syndromic classification but not for age-group stratification (should so)
 > inconsistency of `sympt.start` and `fever.start` before the survey is not checked here (but they are excluded during computation if these date are outside from the computing period)

### Syndromic classification

Each survey is evaluated to fit a syndrome definition 
Consider one boolean column (0/1) for each syndrome type (corresponding to one definition), assigned to each survey response

For each participants, consider age of the last available intake survey [TBD]

Any symptome declared as sudden

*is_sudden* = (`sympt.sudden` not missing and `sympt.sudden` is "Yes") OR (`fever.sudden` not missing and `fever.sudden` is "Yes")

Pain is only accounted if age over 5 (< 120 to exclude inconsistency)

*has_pain* = if age > 5 and age < 120 use `pain` value else consider it`s True

Q6d coding (`highest.temp`)

- "Below 37°C" =0
- "37° - 37.4°C" =1
- "37.5° - 37.9°C"=2
- "38° - 38.9°C"= 3
- "39° - 39.9°C"=4
- 40°C or more"=5
- "I don't know/can't remember"=6

Fever over 39

*fever_level_39* = `highest.temp` not missing and is 4 or 5  (6 is recoded to missing)

Fever over 38

*fever_level_38* = `highest.temp` not missing and is 3, 4 or 5  (6 is recoded to missing)

General set of symptoms for ARI

*general_ari* = any_of[`fever`, `chills`, `asthenia`, `headache` ] OR *has_pain*

Syndromes definitions:

- ili = *is_sudden* and *fever_level_39* and *has_pain* and any_of[`sorethroat`,`cough`,`dyspnea`, `sneeze` ,`rhino`]
- ili.f = *is_sudden* and (`fever` OR *fever_level_39*) and *has_pain* and any_of[`sorethroat`,`cough`,`dyspnea`, `sneeze` ,`rhino`]
- ili.minus = *is_sudden* and `fever` OR *fever_level_38* and (*has_pain* OR `headache`) & any_of[`sorethroat`, `cough`, `dyspnea`]
- ili.minus.fever = *is_sudden* and `fever` and (*has_pain* or `headache`) & any_of[`sorethroat`, `cough`, `dyspnea`]
- ili.who = *is_sudden* & *fever_level_38* & (*has_pain* or `headache`) & any_of[`cough`, `sorethroat`]
  
- ari.ecdc = *is_sudden* & *general_ari* & any_of[`sorethroat`, `cough`, `dyspnea`]
- ari.plus = *is_sudden* & *general_ari* & any_of[`sorethroat`,`cough`,`dyspnea`, `sneeze` ,`rhino`, `sputum`]
- ari = *is_sudden* & *general_ari* & any_of[`cough`,  `rhino`, `sneeze`]

Remarks: Differences with written definition and last implementation: 

  - *is_sudden* was only using sympt.sudden (not fever.sudden)
  - *has_pain* was only accounting on age for ili
  - ili.who was including `dyspnea`

##  Data preparation

Available Parameters:

  - `active.week.before` : number of week before a given week to consider a participant as active
  - `active.week.after` : number of week after a given week to consider a participant as active
  - `active.max.freq` = Maximum delay between 2 surveys (in weeks)
  - `active.min.surveys` = Minimal number of weekly survey during the season
  - `ignore.first.delay` = In days, Ignore the first survey of a if its dated less this delay from the monday of the current progressed week
  - `ignore.first.only.new` = Only ignore first survey for the new participants (if the season is the first of a participant)
  - `exclude.same.delay` = Maximum number of day to consider a same episode

### 1. Exclude same rule [ param=`exclude.same.delay` ]

delay = number of days (computed on truncated date)

> if same.episode is Yes and previous survey has delay < exclude.same.delay
> cancel syndrome report (consider syndrome is not incident)

### 2. Compute onset column

onset = first available date from *fever.start*, *sympt.start*, *survey date*

incidence week = ISO 8601 year week of the onset (caution use the year of the week, not the year of the date strftime %G%V), we use a numeric encoding year * 100 + week number, but date of the monday of the week

### 3. Aggregate syndromes count by week,participant, counting only 1 syndrome kind by person-week 
    (so for each syndrome = if syndrome > 0 then 1 else 0)

### 4. Compute season-wide data by participants (needed to apply selection rules for each week)

  - count of weekly survey during the season
  - week of the first weekly survey during the season
  - week of the last weekly survey during the season
  - maximum delay between two weekly surveys during the season (computed in number of weeks, with one survey by week)

## Incidence computation for a given week `yw` (year-week) in a given season.

### Active participants selection

For a given week, computation has two steps:

 - Select active participants from all the weekly of the season by applying a set of selection rules
 - Count syndromes for the week `yw`

- remove participants for whom the first survey is after `yw`

- Rule *ignore first survey*
  * Ignore participants for whom the first survey is less than `ignore.first.delay` days from the monday of the week `yw` AND is the first season for the participant

- Rule *Active week before* and after* 
  * Include participants with onset in `yw` - `active.week.before`
  * Include participants with onset in `yw` + `active.week.after`

- Rule *minimum surveys count*
  * Include participants with at least `active.min.survey` count during all the season

- Rule *maximum frequency* (Not used in w2_ex2_if2_s2 profile)
  * Include participants with a delay in weeks between two surveys <= `active.max.freq` over the season

### Incidence computation for the week `yw` and a given syndrome definition
Computation can be done using a set of *strata* (for example age-group, regions)

  1. With select active participants: compute active count for the week by *strata*
  2. With weekly surveys for which `onset` week is equal to the currently computed week `yw` and participants is active for the week
   - Count the number of participants with the syndrome by *strata* 
   - At this step, you should have in each strata, syndrome count and active participants for the week `yw`

#### Crude incidence rate
 
 Crude incidence =  total count of participants with the syndrome for the week `yw`/  total active participants (sum in all *strata*) at the week `yw`
 Confidence interval bounds is the poisson exact IC95% computed on total active participants of the week `yw`

#### Adjusted incidence rate by strata 

In each *strata*:

  -  prop_pop : Proportion observed in the general population for the *strata*
  -  rate = prop_pop * count / active
  -  w2 = (prop_pop / active) ^ 2 * count

 Adjusted incidence = sum(rate) over all *strata*
 
 Confidence interval is computed using DKES estimated for adjuster ratio (Fay & Feuer, 1997, Stat In Med (16) p791-801)

   -  total_count = sum(count) of all *strata*
   -  total_w2 = sum(w2) of all *strata*
   -  Up(x, alpha) = quantile of Chisq distribution (1 - alpha / 2, DF=2(x+1))
   -  Lp(x, alpha) = quantile of Chisq distribution (alpha/2, DF=2x)
   -  Upper = rate + (sqrt(total_w2) / sqrt(total_count)) * (Up(total_count, alpha) - total_count)
   -  Lower = rate + (sqrt(total_w2) / sqrt(total_count)) * (Lp(total_count, alpha) - total_count)
