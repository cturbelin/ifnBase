InfluenzaNet Analysis Package
====================

This package provides a set of functions to manipulate data from InfluenzaNet projects. 

It is configurable to use any survey and can be used for a country centered db or for the european db.
The "platform" feature configuration make it abstract from any implementation.

It aims at facilitate analysis and script sharing using some abstraction from the Influenzanet platform used by standardizing some basic data manipulation and provides set of function for common operations.

This package only provides "base" functions & do not provide analysis itself.

Features :

Database abstraction:
  * RODBC & RPostgreSQL can be used transparently
  * Uses DB directly (no need for export, useable in realtime)
  * Handles country level DB and EU level db (with extra column "country")
  * Handles both season based results tables & single table model

* Surveys 
  * Manipulates surveys definition from DB (surveys, questions, options, & translations)
  * Automatic conversion of DB column names (Qxx) to meaningful variable names (!)
  * It contains EU common survey templates but each platform can provide their own survey definitions
    * Proposes a set of names for common questions & coding labels 

* Data Management
  * Coded data can be recoded to meaningful labels (included in survey definition)
  * Common Survey recoding function (recode.intake, recode.weekly) to use more error-proof coding
  * Syndrome computation management (using DB view or pure R functions), handles syndrom groupsets (regroup precomputed syndromes to simpler grouping), Syndrome computation from various definitions (ECDC, French definition)

* Geographic level abstraction:
  * Organization of geographic area and levels can be configured and queried
  * Handles geographic levels hierarchy & navigation (upper, lower level)
  * Population query function (table have to be available in DB) 

* Misc
  * Provide internationalization set of functions `i18n*`() allowing the use of language independent labels, translatable on render time
  * ISO 8601 Week computation & manipulation yearweek number
  * Filesystem path abstraction function init.path(), my.path() to write settings independent programs
  

