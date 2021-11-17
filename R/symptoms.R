
#' No symptome variable name
#'
#' To be used as a constant in the analysis
#' @export
NO_SYMPTOM = 'no.sympt'

#' @export
#' @rdname symptoms
SYMPT_FEVER = 'fever'

#' @export
#' @rdname symptoms
SYMPT_CHILLS = 'chills'

#' @export
#' @rdname symptoms
SYMPT_RHINO = 'rhino'

#' @export
#' @rdname symptoms
SYMPT_SNEEZE = 'sneeze'

#' @export
#' @rdname symptoms
SYMPT_SORETHROAT = 'sorethroat'

#' @export
#' @rdname symptoms
SYMPT_COUGH = 'cough'

#' @export
#' @rdname symptoms
SYMPT_DYSPNEA = 'dyspnea'

#' @export
#' @rdname symptoms
SYMPT_HEADACHE = 'headache'

#' @export
#' @rdname symptoms
SYMPT_PAIN = 'pain'

#' @export
#' @rdname symptoms
SYMPT_CHESTPAIN = 'chestpain'

#' @export
#' @rdname symptoms
SYMPT_ASTHENIA = 'asthenia'

#' @export
#' @rdname symptoms
SYMPT_ANOREXIA = 'anorexia'

#' @export
#' @rdname symptoms
SYMPT_SPUTUM='sputum'

#' @export
#' @rdname symptoms
SYMPT_WATERYEYE = 'wateryeye'

#' @export
#' @rdname symptoms
SYMPT_NAUSEA = 'nausea'

#' @export
#' @rdname symptoms
SYMPT_VOMITING = 'vomiting'

#' @export
#' @rdname symptoms
SYMPT_DIARRHEA = 'diarrhea'

#' @export
#' @rdname symptoms
SYMPT_ABDOPAIN='abdopain'

#' @export
#' @rdname symptoms
SYMPT_OTHER = 'sympt.other'

#' @export
#' @rdname symptoms
SYMPT_SMELL = "loss.smell"

#' @export
#' @rdname symptoms
SYMPT_TASTE = "loss.taste"

#' @export
#' @rdname symptoms
SYMPT_NOSEBLEED = "nose.bleed"

#' Symptoms
#'
#' @description
#' Registered symptoms and constants
#'
#' @rdname symptoms
SYMPTOMS = c(
  SYMPT_FEVER,

  SYMPT_CHILLS,

  SYMPT_RHINO,

  SYMPT_SNEEZE,

  SYMPT_SORETHROAT,

  SYMPT_COUGH,

  SYMPT_DYSPNEA,

  SYMPT_HEADACHE,

  SYMPT_PAIN,

  SYMPT_CHESTPAIN,

  SYMPT_ASTHENIA,

  SYMPT_ANOREXIA,

  SYMPT_SPUTUM,

  SYMPT_WATERYEYE,

  SYMPT_NAUSEA,

  SYMPT_VOMITING ,

  SYMPT_DIARRHEA,

  SYMPT_ABDOPAIN,

  SYMPT_OTHER,

  SYMPT_SMELL,

  SYMPT_TASTE ,

  SYMPT_NOSEBLEED
)

#' Symptoms grouped by topics
#' @export
SYMPTOMS_TOPICS = list(
  other= SYMPT_OTHER,
  sensorial=c(SYMPT_SMELL, SYMPT_TASTE, SYMPT_NOSEBLEED),
  abdo=c(SYMPT_NAUSEA, SYMPT_VOMITING, SYMPT_DIARRHEA, SYMPT_ABDOPAIN),
  upper=c(SYMPT_RHINO, SYMPT_SNEEZE, SYMPT_SORETHROAT, SYMPT_WATERYEYE),
  lower=c(SYMPT_COUGH, SYMPT_DYSPNEA, SYMPT_SPUTUM, SYMPT_CHESTPAIN),
  fever=c(SYMPT_FEVER, SYMPT_CHILLS, SYMPT_PAIN, SYMPT_HEADACHE),
  weakness=c(SYMPT_ASTHENIA, SYMPT_ANOREXIA)
)

