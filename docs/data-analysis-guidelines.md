---
output:
  word_document: default
  html_document: default
  pdf_document: default
---
# InfluenzaNet Data Analysis Guidelines

Author: Clément Turbelin <clement.turbelin@sorbonne-unversite.fr>
Version: 1, 10 Oct 2019

## Data Naming

Data elements (variables) should be named with meaningful names instead of using data names in the database (not safe to use)

Data names used are defined in https://github.com/cturbelin/ifnBase/blob/master/R/templates.R (survey_templates `aliases` entry)

Principles are :
 - All variables output from the same question are named with the same prefix
 - All variables from the same group of question are named with the same prefix (pregnant.*)
 - Keep the names as simple as possible, one word for each part

## Data values recoding

Nominal Qualitative variables are encoded using integer values, sometimes in a counter intuitive way, this encoding is not safe to be used in data analysis.

Recoding each levels to human readable meaningful levels instead of meaningless integer can reduce error, and make analysis more understandable (i.e gender == "female" always more readable than gender == 1). 

These levels should be the same regardless the platform, a unique set of recoding should be defined. Levels can ben translated in a well-spelled language to produce output (tables, graphs).

Some variables should be carefully recoded because encoding are misleading :

In case of Yes/No question, variable can be recoded either in meaningful label or in a boolean values but with the Yes=True, No=False mapping.

old (value in the DB) -> new

Weekly survey:

- sympt.sudden (weekly:Q5) : 0 -> "Yes", 1 -> "No"
- same.episode (weekly:Q2): 0 -> "Yes", 1= "No", 2="DontKnow", 3=[Missing]
- fever.sudden : 0 -> Yes(True), 1-> No (False)
- off.work (Q10b): 0 -> Yes, 1 -> No, 3 -> "other"
- take.temp (Q6c): 0 -> "Yes", 1 -> "DontKnow"
- fever.when (Q6): 0 -> "Yes", 1 -> "DontKnow"
- sympt.when.end (Q4): 0 -> Yes, 1 -> "DontKnow"
- sympt.when (Q3): 0 -> Yes, 1 -> "DontKnow"


Intake survey:

- gender (Q1): 0 -> "male", 1 -> "female"
- vacc.curseason (Q10): 0 -> "Yes", 1 -> "No"
- vacc.lastseason (Q9): 0 -> "Yes", 1 -> "No"
- pregnant (Q12): 0 -> Yes, 1 -> No

## External Data

### Population Data
 Population data source is **Eurostat**

Population of each country are regularly updated and published as yearly datasets, each analysis using population data should indicate the update year used to conduct the analysis for each country.

We recommend to store population along with a identifier of the update (for example the year the update has been done) and the year of the available population update for the country.
Each data entry should have the attributes (or be linked to): country, update year, source year

To be reproducible, it's better to always use a consistent update : avoid updating a country population's data independently. 

Update population for year N
 For each country consider the last update available (not always the year N, some country can have only)
 Store
