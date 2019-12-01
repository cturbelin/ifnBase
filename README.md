InfluenzaNet Analysis Package
====================

This package provides a set of functions to manipulate data from InfluenzaNet projects. 

It is configurable to use any survey and can be used for a country centered db or for the european db.
The "platform" feature configuration make it abstract from any implementation.

It aims at facilitate analysis and script sharing using some abstraction from the Influenzanet platform used by providing set of function for common operations for data manipulation of Influenzanet data.

This package only provides "base" functions but not the analysis itself.

Features :

* Database abstraction:
  * RODBC & RPostgreSQL can be used transparently
  * Uses DB directly (no need for export, useable in realtime)
  * Handles country-specific platform DB and EU level db (handling extra column "country")
  * Handles both season based results tables & single table model (one table with all seasons data)
  * Provides common functions to get various information about participants (number of survey,...)

* Surveys 
  * Manipulates surveys definition from DB (surveys, questions, options, & translations)
  * Automatic conversion of DB column names (Qxx) to meaningful variable names (!)
  * It contains EU common survey templates but each platform can provide their own survey definitions
    * Proposes a set of names for common questions & coding labels 

* Data Management
  * Coded data can be recoded to meaningful labels (included in survey definition)
  * Common Survey recoding function (`recode_intake()`, `recode_weekly()`) to use more error-proof coding
  * Syndrome computation management 
    + Compute syndrome using Influezanet common 2011 & 2012's definition (using the DB view or pure R functions), 
    + handles syndrom groupsets (regroup precomputed syndromes to simpler grouping), 
    + Syndrome computation from various definitions (using `SyndromeProvider`'s classes).

* Geographic level abstraction:
  * Organization of geographic area and levels can be configured and queried
  * Handles geographic levels hierarchy & navigation (upper, lower level)
  * Proposes a db structure to handle population (total & by age-group) data (work in progress)

* Computation:
  * Incidence computation using *Guerrisi et al, EuroSurv, 2018* at weekly & daily resolution.
  * Episode method to identify survey sequence about the same disease episode and strategies to merge surveys data.

* Misc
  * Provides internationalization set of functions `i18n*`() allowing the use of language independent labels, translatable on render time
  * ISO 8601 Week computation & manipulation yearweek number
  * Filesystem path abstraction function init.path(), my.path() to write settings independent programs
  

