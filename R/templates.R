
#' Survey Template definitions
#'
#' Templates of Surveys
#'
#' Template for common surveys : intake & weekly
#'
#' Contains mapping definition and recoding for the common part of InfluenzaNet surveys
#'
#' names:
#' \items{
#'  \item{eu:intake}
#'  \item{eu:weekly}
#' }
#'
#' @seealso \code{\link{platform_define_survey}}
survey_template = function(name) {
  def = survey_templates[[name]]
  if(is.null(def)) {
    stop(paste0("Unknown survey template '",name,"'"))
  }
  def
}

#'
survey_templates = list()

#' create a recoding alias
#'
#' The recoding will be proceeded using the name provided
#' @param name name of recode to use
#' @export
recode_alias = function(name) {
  structure(name, class="recode_alias")
}

allow_override = function(data) {
  attr(data, "allow_override") <- TRUE
}

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
    "hear.work"="Q17_5"
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
    notvac.reason = 'notvac.reason.*'
  ),
  recodes = list(
    activities = c(
        'activity.fulltime'="0",
        'activity.partial'="1",
        'activity.self'="2",
        'activity.student'="3",
        'activity.home'="4",
        'activity.unemployed'="5",
        'activity.sick'="6",
        'activity.retired'="7",
        'activity.other'="8"
      ),
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
      smoker = c(
        'smoker.no'="0",
        'smoker.occas'="1",
        'smoker.dailyfew'="2",
        'smoker.daily'="3",
        'smoker.dkn'="4",
        'smoker.stopped'="5",
        'smoker.juststop'="6"
      )
  )
) # eu:intake

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
    "visit.plan"="Q7_5",
    "visit.emergency"="Q7_3",
    "visit.hospital"="Q7_2",
    "visit.other"="Q7_4",

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
    "antiviro.med"="Q9c_0",

    "antiviro.home"="Q9c_1",

    "antiviro.when"="Q9b",

    # antibio
    "antibio.med"="Q9d_0",
    "antibio.home"="Q9d_1",

    "change.routine"="Q10",

    "off.work"="Q10b",

    "off.workdelay"="Q10c",

    "sympt.cause"="Q11",

    "household.flu"="Q12_multi_row1_col1", # All columns
    "household.meet"="Q13_multi_row1_col1"
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
      "visit.spe",
      "visit.sau",
      "visit.hosp",
      "visit.other"
    ),
    antibio = c("antibio.med", "antibio.home"),
    antiviro = c("antiviro.med", "antiviro.home"),
    # Specials labels
    symptoms= c('no.sympt', 'fever', 'chills','rhino', 'sneeze', 'sorethroat', 'cough', 'dyspnea', 'headache', 'pain', 'chestpain', 'asthenia',
                'anorexia', 'sputum', 'wateryeye', 'nausea', 'vomiting', 'diarrhea', 'abdopain', 'sympt.other')
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
     'other' = 2
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
    sympt.when.end = c('Yes' = 0, 'DNK' = 1),

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

    "visit.delay"   = recode_alias("visit.delay"),
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
