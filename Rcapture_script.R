require(dplyr)
require(Rcapture)
require(FSA)

## Edits Edits Edits

# mrClosed(MARKED,RECAPTURED,#MARKED IN RECAPS, method=)
STM_E <- mrClosed(245,248,3, method="Chapman")
STM_W <- mrClosed(64,65,1, method="Chapman")

mrClosed(245,248,3, method="Bailey")

summary(STM_E, incl.SE=T)
confint(STM_E, verbose=T)

summary(STM_W, incl.SE=T)
confint(STM_W, verbose=T)

STM_W$N
