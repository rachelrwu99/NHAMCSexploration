#Cleaning data_raw
#order columns alphabetically
data_raw_ordered = data_raw[,order(colnames(data_raw))]

data_clean_delete_missing <- data_raw_ordered[!(data_raw_ordered$ADMDIV < 0 | #controversial to keep
                                                  data_raw_ordered$AMBTRANSFER < 0 | #controversial to keep
                                                  data_raw_ordered$RESIDNCE < 0 |
                                                  data_raw_ordered$RESPR < 0 |
                                                  data_raw_ordered$TEMPF < 0 |
                                                  data_raw_ordered$TEMPDF < 0 |
                                                  data_raw_ordered$PULSE < 0 |
                                                  data_raw_ordered$TOTDIAG < 0 |
                                                  data_raw_ordered$TOTPROC < 0 |
                                                  data_raw_ordered$WAITTIME < 0 |
                                                  data_raw_ordered$WAITTIME < 0) |
                                                !is.na(data_raw_ordered$PRESCR1) |
                                                !is.na(data_raw_ordered$PRESCR2) |
                                                !is.na(data_raw_ordered$CONTSUB1) |
                                                !is.na(data_raw_ordered$CONTSUB2), ]

#Delete all the char variables, will not work in the glm_fit, cuts col from 1058 to 521
data_clean_delete_char <- data_clean_delete_missing[, sapply(data_clean_delete_missing, 
                                                             function(x) !is.character(x))]
#Delete certain variables taken post/at discharge
data_clean_delete_var <- data_clean_delete_char %>% 
  select(-c(AGER, AGEDAYS, ADISP, LOS,
            RFV4, RFV5, 
            RFV43D, RFV53D, 
            PRDIAG4, PRDIAG5,
            MRICONTRAST, MED10:MED30,
            GPMED4:GPMED30,
            LEFTBTRI:DIEDED, ADMIT, ADMTPHYS, AGEFL, BLANK1, BLANK2, 
            BLANK3, BLANK4, CPSUM, CSTRATM, COMSTAT1:COMSTAT9,EDWT, 
            HOSPCODE, LBTC, LWBS, MSA, OBSDIS, OBSHOS, 
            PATCODE, PATWT, PULSED, RACERFL, RESPRD, PTONLINEE1: PTONLINEE6,
            RFID, SEXFL, SETTYPE, TEMPDF, TRANNH, TRANOTH, TRANPSYC, VITALSD , YEAR,
            BPDIASD, BPSYSD, NOFU, RETREFFU, NODISP, OTHDISP))

#Delete these variables with >90% NA or -9, no way to impute, ncol/variables 234 to 179
#note SURGDAY is obsolete variable in 2018, so deleting
data_clean_delete_var <- data_clean_delete_var %>%
  select(-c(BOARDED, CAUSE1R:CAUSE3R,
            CONTSUB10:CONTSUB19, CONTSUB20:CONTSUB9, 
            DIAG3R:DIAG5R, PRESCR10:PRESCR19,
            PRESCR20:PRESCR29, PRESCR6:PRESCR9, SURGDAY))

#Delete these variables with 50% NA or -9
data_delete_var <- data_clean_delete_var %>% 
  select(-c(MED4:MED9, PRESCR3:PRESCR5, REGDIV, TOTHRDIVR))

### Create factors from doubles
data_factored <- data_delete_var
keep_double <- c("AGE", "BPDIAS", "BPSYS", 
                 "NUMDIS", "NUMGIV", "NUMMED", "OBSSTAY", 
                 "PAINSCALE", "POPCT", "PULSE", "RESPR", 
                 "TOTCHRON", "TOTDIAG", "TOTPROC", "WAITTIME")

col_names <- names(data_factored)
data_factored[,col_names] <- lapply(data_factored[,col_names] , as.factor)

data_factored[ ,keep_double] <- lapply(data_factored[ , keep_double], as.numeric)

### Imputing means
#Find the means for each column for imputing
means <- apply(data_factored[,keep_double], 2, function(x) {
  mean(x[which(x > 0)])
})
pain_mean <- as.integer(mean(data_factored$PAINSCALE[which(data_factored$PAINSCALE < 11)]))
#for loop to fill keep_double 
data_factored$AGE[is.na(data_factored$AGE) | data_factored$AGE < 0] <- means["AGE"]
data_factored$BPDIAS[is.na(data_factored$BPDIAS) | data_factored$BPDIAS < 2] <- means["BPDIAS"]
data_factored$BPSYS[is.na(data_factored$BPSYS) | data_factored$BPSYS < 2] <- means["BPSYS"]

data_factored$PAINSCALE[data_factored$PAINSCALE > 10] <- pain_mean
data_factored$POPCT[is.na(data_factored$POPCT) | data_factored$POPCT < 0] <- means["POPCT"]
data_factored$PULSE[is.na(data_factored$PULSE) | data_factored$PULSE < 0] <- means["PULSE"]
data_factored$RESPR[is.na(data_factored$RESPR) | data_factored$RESPR < 0] <- means["RESPR"]
data_factored$TOTCHRON[is.na(data_factored$TOTCHRON) | data_factored$TOTCHRON < 0] <- means["TOTCHRON"]
data_factored$TOTDIAG[is.na(data_factored$TOTDIAG) | data_factored$TOTDIAG < 0] <- means["TOTDIAG"]
data_factored$TOTPROC[is.na(data_factored$TOTPROC) | data_factored$TOTPROC < 0] <- means["TOTPROC"]
data_factored$WAITTIME[is.na(data_factored$WAITTIME) | data_factored$WAITTIME < 0] <- means["WAITTIME"]

#adding 0 level to CONTSUB1, CONTSUB2, PRESC1, PRESC2, for NA rows then refactoring 
data_factored$CONTSUB1[is.na(data_factored$CONTSUB1)] <- 0
data_factored$CONTSUB2[is.na(data_factored$CONTSUB2)] <- 0

#Unfactor PRESCR1 and 2
data_factored$PRESCR1 <- as.numeric(data_factored$PRESCR1)
data_factored$PRESCR1[is.na(data_factored$PRESCR1)] <- 0
data_factored$PRESCR2 <- as.numeric(data_factored$PRESCR2)
data_factored$PRESCR2[is.na(data_factored$PRESCR2)] <- 0
data_factored$PRESCR1 <- as.factor(data_factored$PRESCR1)
data_factored$PRESCR2 <- as.factor(data_factored$PRESCR2) 

# dropping rows with NA values
data_factored = data_factored %>%
  drop_na()