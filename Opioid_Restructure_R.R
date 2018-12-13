library(tidyverse)
op <- opioids_workingcopy
Spec_Test <- op %>%
  select(everything()) %>%
  group_by(Specialty) %>%
  count("Abilify")

SpTest2 <- op %>%
  select(-NPI, -Gender, -State, -Credentials, -Opioid.Prescriber) %>%
  group_by(Specialty) %>%
  summarise(Abilify = sum(Abilify))

opnorm <- opioids_workingcopy
#drop credentials 
opnorm <- opnorm %>%
  select(-Credentials)
#creating dummy variables for normalization of dataset
library(dummies)
#create 'state' dummy
opnorm <- cbind(opnorm, dummy(opnorm$State, sep = "_"))
#create 'gender' dummy
opnorm <- cbind(opnorm, dummy(opnorm$Gender, sep = "_"))
#create 'specialty' dummy
opnorm <- cbind(opnorm, dummy(opnorm$Specialty, sep = "_"))


#counting number of specialties
spc <- op %>%
  count(Specialty) %>%
  arrange(desc(n))

pres <- op %>%
  select(-NPI, -State, -Gender, -Specialty, -Credentials) %>%
  count(everything()) %>%
  arrange(desc(n))

#Revisionist Opioid.Prescriber
revop <- opioids_workingcopy
#First, want to know average number of opioids per a distinct specialty
avgNP <- revop %>%
  select(-Gender, - State, -Credentials, -Opioid.Prescriber)
#Chose to look at the Nurse Practitioner specialty, which has 238 records
avgNP <- avgNP %>%
  select(NPI, Specialty, ACETAMINOPHEN.CODEINE, FENTANYL, GABAPENTIN, HYDROCODONE.ACETAMINOPHEN,
         HYDROMORPHONE.HCL, METHADONE.HCL, MORPHINE.SULFATE, OXYCODONE.ACETAMINOPHEN,
         OXYCODONE.HCL, OXYCONTIN, TRAMADOL.HCL) %>%
  filter(avgNP$Specialty == "Nurse Practitioner")
#Now want to know the Mean of each opioid prescription
avgOp <- avgNP %>%
  group_by(Specialty) %>%
  summarise(avg_ACETAMINOPHEN.CODEINE = mean(ACETAMINOPHEN.CODEINE),
            avg_FENTANYL = mean(FENTANYL),
            avg_GABAPENTIN = mean(GABAPENTIN),
            avg_HYDROCODONE.ACETAMINOPHEN = mean(HYDROCODONE.ACETAMINOPHEN),
            avg_HYDROMORPHONE.HCL = mean(HYDROMORPHONE.HCL),
            avg_METHADONE.HCL = mean(METHADONE.HCL),
            avg_MORPHINE.SULFATE = mean(MORPHINE.SULFATE), 
            avg_OXYCODONE.ACETAMINOPHEN = mean(OXYCODONE.ACETAMINOPHEN),
            avg_OXYCODONE.HCL = mean(OXYCODONE.HCL),
            avg_OXYCONTIN = mean(OXYCONTIN),
            avg_TRAMADOL.HCL = mean(TRAMADOL.HCL))
#Add all opioids together and get the average
average_opioid <- avgNP %>%
  select(everything()) %>%
  mutate(Average.Opioid = sum(avgOp$avg_ACETAMINOPHEN.CODEINE,
                              avgOp$avg_FENTANYL,
                              avgOp$avg_GABAPENTIN,
                              avgOp$avg_HYDROCODONE.ACETAMINOPHEN,
                              avgOp$avg_HYDROMORPHONE.HCL,
                              avgOp$avg_METHADONE.HCL,
                              avgOp$avg_MORPHINE.SULFATE,
                              avgOp$avg_OXYCODONE.ACETAMINOPHEN,
                              avgOp$avg_OXYCODONE.HCL,
                              avgOp$avg_OXYCONTIN,
                              avgOp$avg_TRAMADOL.HCL))

#Then create another column to add up a prescriber's total opioid prescriptions
NPopioid <- average_opioid %>%
  mutate(Personal.Opioid = avgNP$ACETAMINOPHEN.CODEINE +
                               avgNP$FENTANYL +
                               avgNP$GABAPENTIN +
                               avgNP$HYDROCODONE.ACETAMINOPHEN +
                               avgNP$HYDROMORPHONE.HCL +
                               avgNP$METHADONE.HCL +
                               avgNP$MORPHINE.SULFATE +
                               avgNP$OXYCODONE.ACETAMINOPHEN +
                               avgNP$OXYCODONE.HCL +
                               avgNP$OXYCONTIN +
                               avgNP$TRAMADOL.HCL)

#Now we want to use an ifelse function to create a revisionist Opioid.Prescriber Boolean variable
Op.Pres.Revision <- NPopioid %>%
  select(everything()) %>%
  mutate(Overprescribing = ifelse(Personal.Opioid > Average.Opioid, "1", "0"))

write.xlsx(Op.Pres.Revision, 'OpioidPrescriber_Revision.xlsx')
