##### Supplemental code used for the typological survey reported in
################## Milenkovic, A. Serbian Monosyllabic Lengthening as a superadditive gang effect, Poster presented at CLS 59, April 30,2023

##### Factorial typology of the Trochaic Quantity constraint proposed in Zec (1999) to block vowel lengthening in High-toned monosyllables in Serbian
##### This constraint penalizes trochaic feet whose head syllable/mora is higher-pitched than the nonhead syllable/mora
##### Ultimately, Zec (1999)'s Trochaic Quantity constraint predicts a number of pathological interactions
##### The factorial typology of this constraint was assessed using the OT-Help 2.0 software (Staubs et al. 2010)
##### The following code identifies pathological mappings in the output grammrs and classifies the grammars based on their pathological profile


###############################################################

library(dplyr)

### Constraint set, candidates and their violation profiles submitted to OT-Help
file_url0 <- "https://raw.githubusercontent.com/AljosaMilenkovic/cls-supplemental-files/main/factorial%20typology%20trochaic_quantity.txt"
constraints <- read.csv(file_url0, header=TRUE, sep = "\t", encoding = "UTF-8")

### Output of OT-Help; grammars predicted by the constraint set 
file_url <- "https://raw.githubusercontent.com/AljosaMilenkovic/cls-supplemental-files/main/grammars.csv"
grammars <- read.csv(file_url, header = TRUE, sep = ",", encoding = "UTF-8")


######################## Pathological deletion of High tone in the head position of a bimoraic trochee
grammars <- grammars %>% mutate(h_deletion = ifelse(grepl("\\.\\(a", a.A) | 
                                          grepl("\\.\\(a", a.Aa) | 
                                          grepl("\\(a", Aa) | 
                                          grepl("\\(aa\\)\\.", A.a) | 
                                          grepl("\\(aa\\)\\.", Aa.a) | 
                                          grepl("\\(aa\\)\\.", Aa.aa) | 
                                          grepl("\\(a\\.", A.a) | 
                                          grepl("\\(a\\.", Aa.a) | 
                                          grepl("\\(a\\.", A.aa), 1,0))
sum(grammars$h_deletion)

############## Pathological Tonal  Flop
grammars <- grammars %>% mutate(flop = ifelse(grepl("A\\.\\(aa\\)", a.Aa) | 
                                    grepl("\\(aa\\)\\.A", Aa.a),1,0))
sum(grammars$flop)

############# Pathological shortening: Stressed heavies with a High-toned first mora undergo shortening in response to Trochaic Quantity

grammars <- grammars %>% mutate(shortening = ifelse(grepl("A\\)", Aa) |
                                          grepl("\\(a\\)", Aa) |
                                          grepl("A\\)", aa) |
                                          grepl("\\(A\\)\\.", aa.a) |
                                          grepl("\\(A\\)\\.", Aa.a) |
                                          grepl("\\.\\(A\\)", a.Aa) |
                                          grepl("\\(A\\)\\.", Aa.aa) |
                                          grepl("\\(A\\)\\.", aa.aa) |
                                          grepl("\\.\\(A\\)", aa.Aa),1,0))
sum(grammars$shortening)


############### Avoidance effects

grammars <- grammars %>% mutate(avoidance = ifelse(grepl("\\(A\\)\\.", a.a) | 
                                         grepl("\\(A\\)\\.", A.a) |
                                         grepl("\\.\\(a", A.a) |
                                         grepl("\\(aa\\)\\.", A.a) |
                                         grepl("\\.\\(A\\)", aa.a) |
                                         grepl("\\.\\(a", Aa.a) |
                                         grepl("aa\\.\\(A\\)", Aa.a) |
                                         grepl("\\(A\\)\\.aa", a.aa) |
                                         grepl("a\\)\\.A", a.Aa) |
                                         grepl("\\(A\\)\\.", a.Aa) |
                                         grepl("\\.\\(aa\\)", Aa.aa) |
                                         grepl("\\(aa\\)\\.A", aa.Aa),1,0))

################# Identifies grammars' pathological profile
#################   F = Pathological Tonal Flop
#################   S = Pathological Vowel Shortening
#################   D = Pathological High Deletion
#################   A = Pathological Avoidance effects

grammars <- grammars %>% mutate(pathology = if_else((h_deletion == 1 & flop == 1 & shortening == 1 & avoidance == 1), "all",
                                        if_else((h_deletion == 1 & flop == 0 & shortening == 1 & avoidance == 1), "S+D+A",
                                                if_else((h_deletion == 1 & flop == 1 & shortening == 0 & avoidance == 1), "F+D+A",
                                                        if_else((h_deletion == 0 & flop == 1 & shortening == 1 & avoidance == 1), "F+S+A",
                                                                if_else((h_deletion == 1 & flop == 1 & shortening == 1 & avoidance == 0), "F+S+D",
                                                                        if_else((h_deletion == 1 & flop == 0 & shortening == 0 & avoidance == 1), "D+A",
                                                                                if_else((h_deletion == 0 & flop == 0 & shortening == 1 & avoidance == 1), "S+A",
                                                                                        if_else((h_deletion == 1 & flop == 0 & shortening == 1 & avoidance == 0), "S+D",
                                                                                                if_else((h_deletion == 0 & flop == 1 & shortening == 0 & avoidance == 1), "F+A",
                                                                                                        if_else((h_deletion == 1 & flop == 1 & shortening == 0 & avoidance == 0), "F+D",
                                                                                                                if_else((h_deletion == 0 & flop == 0 & shortening == 0 & avoidance == 1), "A",
                                                                                                                        if_else((h_deletion == 1 & flop == 0 & shortening == 0 & avoidance == 0), "D",
                                                                                                                                if_else((h_deletion == 0 & flop == 0 & shortening == 1 & avoidance == 0), "S",
                                                                                                                                        if_else((h_deletion == 0 & flop == 1 & shortening == 0 & avoidance == 0), "F", "none")))))))))))))))

################ Summary statistics
grammars_sum <- grammars %>% group_by(pathology) %>% summarise(count = n())
grammars_sum1 <- grammars %>% group_by(pathology, solution) %>% summarise(count = n())