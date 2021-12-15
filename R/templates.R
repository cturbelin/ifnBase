# Templates related functions

#' Survey Template definitions
#'
#' Templates of Surveys
#'
#' Template for common surveys : intake & weekly
#'
#' Contains mapping definition and recoding for the common part of InfluenzaNet surveys
#'
#' names:
#' \itemize{
#'  \item{eu:intake}
#'  \item{eu:weekly}
#'  \item{eu:vaccination}
#' }
#'
#' @param name name of the template to get
#'
#' @export
#' @seealso \code{\link{platform_define_survey}}
survey_template = function(name) {
  def = survey_templates[[name]]
  if(is.null(def)) {
    stop(paste0("Unknown survey template '",name,"'"))
  }
  def
}

#' Create a mapping to recode variable value to labels
#' @param codes vector of values as stored in the database
#' @param labels vector of labels
as_mapping = function(codes, labels) {
  if(length(codes) != length(labels)) {
    rlang::abort("codes and labels must have the same length")
  }
  if(anyDuplicated(codes)) {
    rlang::abort("codes values must be unique", codes=codes)
  }
  if(anyDuplicated(labels)) {
    rlang::abort("labels values must be unique", labels=labels)
  }
  codes = as.vector(codes)
  names(codes) <- labels
  codes
}


labels_activities = c(
  'activity.fulltime'="0",
  'activity.partial'="1",
  'activity.self'="2",
  'activity.student'="3",
  'activity.home'="4",
  'activity.unemployed'="5",
  'activity.sick'="6",
  'activity.retired'="7",
  'activity.other'="8"
)

# Labels Yes/No/DontKnow with Yes=0 and No=1 (historical form for good or bad)
labels_ynp = as_mapping(c(0:2), YES_NO_DNK)

# Labels Yes/No/DontKnow with Yes=1
labels_y1np = c('Yes'=1,'No'=0,'DNK'=2)

covid.vaccine.list = list(
  "same"=0,
  "pfizer"=1,
  "moderna"=2,
  "astra"=3,
  "jonhson"=4,
  "DNK"=99
)

covid.vaccine.doses = list(
    "one"=1,
    "two"=2,
    "more_2"=3, # First version
    "three"=4, # Second version
    "more_3"=5, # Second version
    "DNK"=99
)


from_2020 = rlang::quo(season >= 2020)

#' survey templates list
#' @noRd
survey_templates = list()

survey_templates[["eu:intake"]] = list(
  geo.column="Q3",

  aliases=list(

    "for.whom"="Q0",
    "code_zip"="Q3",
    "gender"="Q1",
    "date.birth"="Q2",
    "main.activity"="Q4",
    "occup.place"="Q4b",
    "occup.place.location"="Q4b_0_open",
    "occupation"="Q4c",
    "transport"="Q7",
    "time.transport"="Q7b",
    "often.ili"="Q8",

    # Education
    "education.noqualif"="Q4d_0",
    "education.gcse"="Q4d_1",
    "education.alevel"="Q4d_2",
    "education.bachelor"="Q4d_3",
    "education.higher"="Q4d_4",
    "education.student"="Q4d_5",

    # Contact type
    "contact.children"="Q5_0",
    "contact.elder"="Q5_1",
    "contact.patient"="Q5_2",
    "contact.crowd"="Q5_3",
    "contact.none"="Q5_4",

    # houseold
    "house.0"="Q6_0", # 0-4 yo
    "house.5"="Q6_1", # 5- 18 yo
    "house.19"="Q6_2", # 19- 44 yo
    "house.45"="Q6_3", # 45- 64 yo
    "house.65"="Q6_4", # 45- 64 yo

    "house.0.num"="Q6_0_open", # 0-4 yo
    "house.5.num"="Q6_1_open", # 5- 18 yo
    "house.19.num"="Q6_2_open", # 19- 44 yo
    "house.45.num"="Q6_3_open", # 45- 64 yo
    "house.65.num"="Q6_4_open", # 45- 64 yo

    "daycare.number"="Q6b", # Number of children in daycare

    # Vaccination
    "vacc.lastseason"="Q9",
    "vacc.curseason"="Q10",
    "vacc.when"="Q10b",
    "vacc.date"="Q10b_1_open",

    # "vacc.reason"="Q10c",
    "vacc.reason.risk"="Q10c_0",
    "vacc.reason.myrisk"="Q10c_1", # decrease my risk
    "vacc.reason.spread"="Q10c_2", # decrease risk of spreading
    "vacc.reason.doctor"="Q10c_3", # doctor recommended it
    "vacc.reason.work"="Q10c_4", # work & school
    "vacc.reason.available"="Q10c_5", # vaccination was available
    "vacc.reason.free"="Q10c_6", # vaccin was free
    "vacc.reason.miss"="Q10c_7", # I don't want to miss work/school
    "vacc.reason.always"="Q10c_8", # I always get the vaccine
    "vacc.reason.other"="Q10c_9", # I always get the vaccine
    "vacc.reason.covid19"=variable_available("Q10c_12", quo(season > 2020)),

    # "notvacc.reason"="Q10d",
    "notvac.reason.plan"="Q10d_0",
    "notvac.reason.offer"="Q10d_1", # Haven't been offered
    "notvac.reason.risk"="Q10d_2", # Don't belong to risk group
    "notvac.reason.own"="Q10d_3", # Better to get own immunity
    "notvac.reason.doubt"="Q10d_4", # Doubt about vaccine effectiveness
    "notvac.reason.minor"="Q10d_5", # Influenza is a minor illness
    "notvac.reason.likely"="Q10d_6", # I don't think I am likely to get influenza
    "notvac.reason.cause"="Q10d_7", # I believe that influenza vaccine can cause influenza
    "notvac.reason.safety"="Q10d_8", # I am worried that the vaccine is not safe or will cause illness or other adverse events
    "notvac.reason.vaccin"="Q10d_9", # I don't like having vaccinations
    "notvac.reason.available"="Q10d_10", # The vaccine is not readily available to me
    "notvac.reason.free"="Q10d_11", # The vaccine is not free
    "notvac.reason.no"="Q10d_12", # no particular reason
    "notvac.reason.doctor"="Q10d_13", # Although my doctor recommended a vaccine, I did not get one
    "notvac.reason.other"="Q10d_14", #

    # Q11 Condition
    "condition.none"="Q11_0",
    "condition.asthma"="Q11_1",
    "condition.diabetes"="Q11_2",
    "condition.lung"="Q11_3",
    "condition.heart"="Q11_4",
    "condition.kidney"="Q11_5",
    "condition.immune"="Q11_6",
#    "condition.noanwser"="Q11_7", # only in some countries

    # Q12 Pregnant
    "pregnant"="Q12",
    "pregnant.trim"="Q12b",

    "smoker"="Q13",

    # Q14 Allergy
    "allergy.hay"="Q14_1", # Hay fever
    "allergy.dust"="Q14_2", # Home dust mite
    "allergy.pets"="Q14_3", # Domestic pets
    "allergy.other"="Q14_4", # Other
    "allergy.none"="Q14_5", # Hay fever

    # Q15 Diet
    "diet.normal"="Q15_0",
    "diet.vegetarian"="Q15_1",
    "diet.vegan"="Q15_2",
    "diet.lowcal"="Q15_3",
    "diet.other"="Q15_4",

    # Q16 Pets
    "pets.none"="Q16_0",
    "pets.dog"="Q16_1",
    "pets.cat"="Q16_2",
    "pets.bird"="Q16_3",
    "pets.other"="Q16_4",

    # Hear about us
    "hear.radio"="Q17_0",
    "hear.newspaper"="Q17_1",
    "hear.internet"="Q17_2",
    "hear.poster"="Q17_3",
    "hear.family"="Q17_4",
    "hear.work"="Q17_5",

    'covid.vaccine'=variable_available("Q35", from_2020),
    'covid.vaccine.which'=variable_available("Q35b", from_2020),
    'covid.vaccine.doses'=variable_available("Q35c", from_2020),
    "covid.vaccine.when1"=variable_available("Q35d", from_2020),
    "covid.vaccine.when1.date"=variable_available("Q35d_1_open", from_2020),
    "covid.vaccine.when2"=variable_available("Q35e", from_2020),
    "covid.vaccine.when2.date"=variable_available("Q35e_1_open", from_2020),
    "covid.vaccine.which2"=variable_available("Q35g", from_2020),

    "covid.vacc.reason.risk"=variable_available("Qcov35f_0", from_2020), # At risk of Complication
    "covid.vacc.reason.get"=variable_available("Qcov35f_1", from_2020), # Reduce risk of getting
    "covid.vacc.reason.transm"=variable_available("Qcov35f_2", from_2020), # Reduce risk of transmission
    "covid.vacc.reason.doctor"=variable_available("Qcov35f_3", from_2020), # Recommended by doctor
    "covid.vacc.reason.work"=variable_available("Qcov35f_4", from_2020), # Recommended by workplace/school
    "covid.vacc.reason.avail"=variable_available("Qcov35f_5", from_2020), # Available
    "covid.vacc.reason.free"=variable_available("Qcov35f_6", from_2020), # Free
    "covid.vacc.reason.miss"=variable_available("Qcov35f_7", from_2020), # Dont want to miss work
    "covid.vacc.reason.always"=variable_available("Qcov35f_8", from_2020), # I always get the vaccine
    "covid.vacc.reason.other"=variable_available("Qcov35f_9", from_2020), # Other
    "covid.vacc.reason.other.txt"=variable_available("Qcov35f_9_open", from_2020), # Other text
    "covid.vacc.reason.close"=variable_available("Qcov35f_20", from_2020), # I work with people in close contact
    "covid.vacc.reason.pha"=variable_available("Qcov35f_21", from_2020) # Recommended By Public Health

  ),
  labels=list(
    # Patterns
    hear.about    = "hear.*",
    education     = 'education.*',
    condition     = 'condition.*',
    allergy       = "allergy.*",
    daycare       = 'daycare.*',
    contact       = 'contact.*',
    diet          = 'diet.*',
    pets          = 'pets.*',
    house         = 'house.*',
    vacc.reason   = 'vacc.reason.*',
    notvac.reason = 'notvac.reason.*',
    covid.vacc.reason="covid.vacc.reason.*"
  ),
  recodes = list(

    gender = c(
      'male'='0',
      'female'='1'
    ),

    activities = labels_activities,

    main.activity = labels_activities,

    occupation = c(
        'occupation.prof' = "0",
        'occupation.office' = "1",
        'occupation.shop' ="2",
        'occupation.worker'="3",
        'occupation.omanual'="4",
        'occupation.other'="5"
      ),
      transport  = c(
        'transport.walk'="0",
        'transport.bike'="1",
        'transport.scooter'="2",
        'transport.car'="3",
        'transport.public'="4",
        'transport.other'="5"
      ),

      time.transport = c(
        'transtime.no'="0",
        'transtime.half'="1",
        'transtime.hour'="2",
        'transtime.less4h'="3",
        'transtime.more4h'="4"
      ),

      often.ili = c(
        'often.never'="0",
        'often.once'="1",
        'often.3'="2",
        'often.6'="3",
        'often.10'="4",
        'often.dkn'="5"
      ),

    'pregnant'= labels_ynp,

    'vacc.lastseason'=labels_ynp,

    'vacc.curseason'=labels_ynp,

    smoker = c(
        'smoker.no'="0",
        'smoker.occas'="1",
        'smoker.dailyfew'="2",
        'smoker.daily'="3",
        'smoker.dkn'="4",
        'smoker.stopped'="5",
        'smoker.juststop'="6"
      ),
    "covid.vaccine"=labels_y1np,
    "covid.vaccine.which"=covid.vaccine.list,
    "covid.vaccine.which2"=covid.vaccine.list,
    covid.vaccine.doses=covid.vaccine.doses
  )
) # eu:intake

from_2021 = rlang::quo(season >= 2021)

survey_templates[['eu:vaccination']] = list(
  aliases=list(
    # Flu Vaccination
    "vacc.lastseason"="Q9",
    "vacc.curseason"="Q10",
    "vacc.when"="Q10b",
    "vacc.date"="Q10b_1_open",

    # Flu vaccination reasons
    "vacc.reason.risk"="Q10c_0",
    "vacc.reason.myrisk"="Q10c_1", # decrease my risk
    "vacc.reason.spread"="Q10c_2", # decrease risk of spreading
    "vacc.reason.doctor"="Q10c_3", # doctor recommended it
    "vacc.reason.work"="Q10c_4", # work & school
    "vacc.reason.available"="Q10c_5", # vaccination was available
    "vacc.reason.free"="Q10c_6", # vaccin was free
    "vacc.reason.miss"="Q10c_7", # I don't want to miss work/school
    "vacc.reason.always"="Q10c_8", # I always get the vaccine
    "vacc.reason.other"="Q10c_9", # I always get the vaccine
    "vacc.reason.covid19"=variable_available("Q10c_12", quo(season > 2020)),

    # Flu not vaccination reasons
    "notvac.reason.plan"="Q10d_0",
    "notvac.reason.offer"="Q10d_1", # Haven't been offered
    "notvac.reason.risk"="Q10d_2", # Don't belong to risk group
    "notvac.reason.own"="Q10d_3", # Better to get own immunity
    "notvac.reason.doubt"="Q10d_4", # Doubt about vaccine effectiveness
    "notvac.reason.minor"="Q10d_5", # Influenza is a minor illness
    "notvac.reason.likely"="Q10d_6", # I don't think I am likely to get influenza
    "notvac.reason.cause"="Q10d_7", # I believe that influenza vaccine can cause influenza
    "notvac.reason.safety"="Q10d_8", # I am worried that the vaccine is not safe or will cause illness or other adverse events
    "notvac.reason.vaccin"="Q10d_9", # I don't like having vaccinations
    "notvac.reason.available"="Q10d_10", # The vaccine is not readily available to me
    "notvac.reason.free"="Q10d_11", # The vaccine is not free
    "notvac.reason.no"="Q10d_12", # no particular reason
    "notvac.reason.doctor"="Q10d_13", # Although my doctor recommended a vaccine, I did not get one
    "notvac.reason.other"="Q10d_14", #

    'covid.vaccine'="Q35",
    'covid.vaccine.which'="Q35b", # Vaccine brand for 1st dose
    'covid.vaccine.doses'="Q35c",
    "covid.vaccine.when1"="Q35d",
    "covid.vaccine.when1.date"="Q35d_1_open",
    "covid.vaccine.when2"="Q35e",
    "covid.vaccine.when2.date"="Q35e_1_open",
    "covid.vaccine.which2"="Q35g",

    "covid.vacc.reason.risk"="Q35f_0", # At risk of Complication
    "covid.vacc.reason.get"="Q35f_1", # Reduce risk of getting
    "covid.vacc.reason.transm"="Q35f_2", # Reduce risk of transmission
    "covid.vacc.reason.doctor"="Q35f_3", # Recommended by doctor
    "covid.vacc.reason.work"="Q35f_4", # Recommended by workplace/school
    "covid.vacc.reason.avail"="Q35f_5", # Available
    "covid.vacc.reason.free"="Q35f_6", # Free
    "covid.vacc.reason.miss"="Q35f_7", # Dont want to miss work
    "covid.vacc.reason.always"="Q35f_8", # I always get the vaccine
    "covid.vacc.reason.other"="Q35f_9", # Other
    "covid.vacc.reason.close"="Q35f_20", # I work with people in close contact
    "covid.vacc.reason.pha"="Q35f_21", # Recommended By Public Health
    "covid.vacc.reason.mandatory"="Q35f_22",
    "covid.vacc.reason.pass"="Q35f_23 ",
    "covid.vacc.reason.other.txt"="Q35f_9_open", # Other text

    "covid.vacc.reason.risk"="Q35f_0", # At risk of Complication
    "covid.vacc.reason.get"="Q35f_1", # Reduce risk of getting
    "covid.vacc.reason.transm"="Q35f_2", # Reduce risk of transmission
    "covid.vacc.reason.doctor"="Q35f_3", # Recommended by doctor
    "covid.vacc.reason.work"="Q35f_4", # Recommended by workplace/school
    "covid.vacc.reason.avail"="Q35f_5", # Available
    "covid.vacc.reason.free"="Q35f_6", # Free
    "covid.vacc.reason.miss"="Q35f_7", # Dont want to miss work
    "covid.vacc.reason.always"="Q35f_8", # I always get the vaccine
    "covid.vacc.reason.other"="Q35f_9", # Other
    "covid.vacc.reason.close"="Q35f_20", # I work with people in close contact
    "covid.vacc.reason.pha"="Q35f_21", # Recommended By Public Health
    "covid.vacc.reason.mandatory"="Q35f_22",
    "covid.vacc.reason.pass"="Q35f_23 ",
    "covid.vacc.reason.other.txt"="Q35f_9_open", # Other text

    # New questions for vaccination v2 (only available from 2021 but depends on country for real availability)
    covid.vaccine.which.pfizer=variable_available("Q35i_1", from_2021),
    covid.vaccine.which.moderna=variable_available("Q35i_2", from_2021),
    covid.vaccine.which.astra=variable_available("Q35i_3", from_2021),
    covid.vaccine.which.jonhson=variable_available("Q35i_4", from_2021),
    covid.vaccine.which.dnk=variable_available("Q35i_99", from_2021),

    covid.vaccine.last.when=variable_available("Q35j", from_2021),
    covid.vaccine.last.date=variable_available("Q35j_1_open", from_2021),

    covid.vaccine.second.plan=variable_available("Q35k", from_2021),

    covid.vacc.one.reason=variable_available("Q35l", from_2021), # Reason fon single dose

    "covid.vacc.one.reason.other"=variable_available("Q35l_6_open", from_2021),

    covid.nvac.reason.plan =variable_available("Q35m_0", from_2021), # I plan to get the vaccine
    covid.nvac.reason.notproposed =variable_available("Q35m_1", from_2021), # Not proposed
    covid.nvac.reason.pregnant.disc =variable_available("Q35m_15", from_2021), #  Adviced to get the vaccine because I'm pregnant
    covid.nvac.reason.pregnant.fear =variable_available("Q35m_16", from_2021), #  Pregnant and fear for my baby 16
    covid.nvac.reason.notriskgroup =variable_available("Q35m_2", from_2021), #  Dont belong to a risk group 2
    covid.nvac.reason.natural =variable_available("Q35m_3", from_2021), #  Better to get their own natural immunity 3
    covid.nvac.reason.efficacy =variable_available("Q35m_4", from_2021), #  Doubt about efficacy 4
    covid.nvac.reason.benign =variable_available("Q35m_5", from_2021), #  Covid is not a severe disease 5
    covid.nvac.reason.avoid.hs =variable_available("Q35m_17", from_2021), #  Avoid health seeking because of the pandemic 17
    covid.nvac.reason.unlikely =variable_available("Q35m_6", from_2021), #  Not suspeptible 6
    covid.nvac.reason.cause.covid =variable_available("Q35m_7", from_2021), #  Vaccine can cause the disease 7
    covid.nvac.reason.adverse =variable_available("Q35m_8", from_2021), #  Fear of adverse event, Not safe 8
    covid.nvac.reason.dontlike =variable_available("Q35m_9", from_2021), #  Dont like to get vaccine 9
    covid.nvac.reason.accessible =variable_available("Q35m_10", from_2021), #  Not available for me 10
    covid.nvac.reason.disagree =variable_available("Q35m_20", from_2021), #  Disagree vaccine policy 20
    covid.nvac.reason.other =variable_available("Q35m_14", from_2021), #  Other 14
    covid.nvac.reason.dnk =variable_available("Q35m_12", from_2021), #  Dont know 12

    "covid.nvac.reason.other.txt"=variable_available("Q35m_14_open", from_2021)

  ),
  labels=list(
    "covid.vacc.reason"=var_labels("covid.vacc.reason.*", exclude="covid.vacc.reason.other.txt"),
    "covid.nvac.reason"=var_labels("covid.nvac.reason.*", exclude="covid.nvac.reason.other.txt"),
    vacc.reason   = 'vacc.reason.*',
    notvac.reason = 'notvac.reason.*',
    "covid.vacc.reason"=var_labels("covid.vacc.reason.*", exclude="covid.vacc.reason.other.txt"),
    "covid.nvac.reason"=var_labels("covid.nvac.reason.*", exclude="covid.nvac.reason.other.txt"),
    covid.vaccine.which="covid.vaccine.which.*"
  ),
  recodes=list(
    vacc.curseason=labels_ynp,
    vacc.lastseason=labels_ynp,
    "covid.vaccine"=labels_y1np,
    "covid.vaccine.which"=covid.vaccine.list,
    "covid.vaccine.which2"=covid.vaccine.list,
    "covid.vaccine.doses"=covid.vaccine.doses,
    covid.vaccine.second.plan=labels_y1np,
    covid.vacc.one.reason=c(
      "covid.infection.before"="1",
      "covid.infection.after"="2",
      "covid.jonhson"="3",
      "counter.indication"="4",
      "changed.mind"="5",
      "other"="6",
      "DNK"="0"
    )
  )
)

survey_templates[["eu:weekly"]] = list(
  aliases=list(
    # symptoms
    'no.sympt'='Q1_0',
    'fever'='Q1_1',
    'chills'='Q1_2',
    'rhino'='Q1_3',
    'sneeze'='Q1_4',
    'sorethroat'='Q1_5',
    'cough'='Q1_6',
    'dyspnea'='Q1_7',
    'headache'='Q1_8',
    'pain'='Q1_9',
    'chestpain'='Q1_10',
    'asthenia'='Q1_11',
    'anorexia'='Q1_12',
    'sputum'='Q1_13',
    'wateryeye'='Q1_14',
    'nausea'='Q1_15',
    'vomiting'='Q1_16',
    'diarrhea'='Q1_17',
    'abdopain'='Q1_18',
    'sympt.other'='Q1_19',
    "loss.smell"=variable_available("Q1_23", rlang::quo(season >= 2019)),
    "loss.taste"=variable_available("Q1_21", rlang::quo(season >= 2019)),
    "nose.bleed"=variable_available("Q1_22", rlang::quo(season >= 2019)),

    # About the symptoms
    "same.episode"="Q2",
    "sympt.when"="Q3",
    "sympt.start"="Q3_0_open",
    "sympt.when.end"="Q4",
    "sympt.end"="Q4_0_open",
    'sympt.sudden'="Q5",
    "fever.when"="Q6",
    "fever.start"="Q6_1_open",
    "fever.sudden"="Q6b",
    "take.temp"="Q6c",
    "highest.temp"="Q6d",

    # not used
    "contactmed.reception"="Q8_1",
    "contactmed.doctor"="Q8_2",
    "contactmed.nhs"="Q8_3",
    "contactmed.npfs"="Q8_4",
    "contactmed.no"="Q8_0",
    "contactmed.other"="Q8_5",

    "contactmed.delay"="Q8b",

   # "website"="Q8c",

   "hospitalization"="Q14",

    # visits
   "visit.no"="Q7_0",
   "visit.GP"="Q7_1",
   "visit.hospital"="Q7_2",
   "visit.emergency"="Q7_3",
   "visit.other"="Q7_4",
   "visit.plan"="Q7_5",

    # "visit.delay"="Q7b",

    "visit.delay.GP"="Q7b_multi_row1_col1",
    "visit.delay.hospital"="Q7b_multi_row2_col1",
    "visit.delay.emergency"="Q7b_multi_row3_col1",
    "visit.delay.other"="Q7b_multi_row4_col1",

    # medication
    "medic.no"="Q9_0",
    "medic.pain"="Q9_1",
    "medic.cough"="Q9_2",
    "medic.antiviral"="Q9_3",
    "medic.antibio"="Q9_4",
    "medic.other"="Q9_5",
    "medic.dkn"="Q9_6",

    # Antiviral questions
    #"antiviro.med"="Q9c_0",
    #"antiviro.home"="Q9c_1",

    "antiviro.when"="Q9b",

    # antibio
    #"antibio.med"="Q9d_0",
    #"antibio.home"="Q9d_1",
    "change.routine"="Q10",

    "off.work"="Q10b",

    "off.workdelay"="Q10c",

    "sympt.cause"="Q11"

#    "household.flu"="Q12_multi_row1_col1", # Not in standard
#    "household.meet"="Q13_multi_row1_col1" # Not in standard
  ),

  labels = list(
    medic="medic.*",
    contactmed = c(
      "contactmed.reception",
      "contactmed.doctor",
      "contactmed.nhs",
      "contactmed.npfs",
      "contactmed.no",
      "contactmed.other"
    ),
    visit = c(
      "visit.no",
      "visit.GP",
      "visit.plan",
      "visit.emergency",
      "visit.hospital",
      "visit.other"
    ),
   #  antibio = c("antibio.med", "antibio.home"),
   # antiviro = c("antiviro.med", "antiviro.home"),
    # Specials labels
    symptoms= c('no.sympt', 'fever', 'chills','rhino', 'sneeze', 'sorethroat', 'cough', 'dyspnea', 'headache', 'pain', 'chestpain', 'asthenia',
                'anorexia', 'sputum', 'wateryeye', 'nausea', 'vomiting', 'diarrhea', 'abdopain', "loss.smell", 'loss.taste','nose.bleed', 'sympt.other')
  ),
  recodes = list(
    sympt.cause = c(
      "cause.ili"="0",
      "cause.cold"="1",
      "cause.allergy"="2",
      "cause.gastro"="3",
      "cause.other"="4",
      "cause.dkn"="5",
      "cause.asthma"="6"
    ),
    # Q10
    'change.routine' = c(
      'routine.no'=0,
      'routine.yes'=1,
      'routine.off'=2
    ),

    # Q10b
    off.work = c(
    'Yes' = 0,
     'No' = 1,
     'other' = 3 # Weird coding..
    ),

    # Q10c
    off.workdelay = c(
      '1d' = 0,
      '2d' = 1,
      '3d' = 2,
      '4d' = 3,
      '5d' = 4,
      '6_10d' = 5,
      '11_15d' = 6,
      'over_15' = 7
    ),

    # Q3
    sympt.when = c('Yes' = 0, 'DNK' = 1),

    # Q4
    sympt.when.end = c('Yes' = 0, 'DNK' = 1, 'Still'=2),

    sympt.sudden = c(
      'Yes' = 0,
      'No' = 1,
      'DNK' = 2
    ),

    fever.when = c(
      'Yes' = 0,
      'DNK' = 1
    ),

    # Q6c
    take.temp = c(
      'Yes' = 0,
      'No' = 1,
      'DNK' = 2
    ),

    # Q6d
    highest.temp = c(
      '<37' = 0,
      '[37,37.5)' = 1,
      '[37.5,38)' = 2,
      '[38,39)' = 3,
      '[39,40)' = 4,
      '>40' = 5,
      'DNK' = 6
    ),

    # Q7b
    visit.delay = c(
      '0d' = 0,
      '1d' = 1,
      '2d' = 2,
      '3d' = 3,
      '4d' = 4,
      '5_7d' = 5,
      'over7d' = 6 ,
      'DNK' = 7,
      'Unk' = 100
    ),

    "visit.delay.hospital"  = recode_alias("visit.delay"),
    "visit.delay.emergency"   = recode_alias("visit.delay"),
    "visit.delay.other" = recode_alias("visit.delay"),

    antiviro.when = c(
      'same.day' = 0,
      '1d' = 1,
      '2d' = 2,
      '3d' = 3,
      '4d' = 4,
      '5-7d' = 5,
      'over7d' = 6,
      'DNK' = 7
    ),

    same.episode = c(
     'Yes' = 0,
     'No' = 1,
     'DNK' = 2
    )
  )
)

# Remove not useful anymore
rm(labels_ynp)
rm(labels_activities)
