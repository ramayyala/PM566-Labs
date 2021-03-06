Assignment_3
================

# API’s

## Download the Data

``` r
library(data.table)
library(stringr)
library(rvest)
library(xml2)
library(httr)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v forcats 0.5.1
    ## v readr   2.0.2

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::between()        masks data.table::between()
    ## x dplyr::filter()         masks stats::filter()
    ## x dplyr::first()          masks data.table::first()
    ## x readr::guess_encoding() masks rvest::guess_encoding()
    ## x dplyr::lag()            masks stats::lag()
    ## x dplyr::last()           masks data.table::last()
    ## x purrr::transpose()      masks data.table::transpose()

``` r
library(tidytext)
library(tibble)
library(forcats)
```

## Using the NCBI API, look for papers that show up under the term “sars-cov-2 trial vaccine.” Look for the data in the pubmed database, and then retrieve the details of the paper as shown in lab 7. How many papers were you able to find?

``` r
# Downloading the website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine")

# Finding the counts
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]/span")

# Turning it into text
counts <- as.character(counts)

# Extracting the data using regex
stringr::str_extract(counts, "[0-9,]+")
```

    ## [1] "2,335"

``` r
stringr::str_replace(counts, "[^[:digit:]]+([[:digit:]]+),([[:digit:]]+)[^[:digit:]]+","\\1\\2")
```

    ## [1] "2335"

I was able to find 2,335 papers under the term “sars-cov-2 trial
vaccine”.

## Using the list of pubmed ids you retrieved, download each papers’ details using the query parameter rettype = abstract. If you get more than 250 ids, just keep the first 250.

``` r
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(db = "pubmed",term= "sars-cov-2 trial vaccine",retmax=1000)
)
ids <- httr::content(query_ids)
```

``` r
# Turn the result into a character vector
ids <- as.character(ids)

# Find all the ids 
ids <- stringr::str_extract_all(ids, "<Id>[[:digit:]]+</Id>")[[1]]

# Remove all the leading and trailing <Id> </Id>. Make use of "|"
ids <- stringr::str_remove_all(ids, "<Id>|</Id>")
# Keep first 250 ids
ids <- ids[1:250]
#Get the Publications
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
  query = list(
    db = "pubmed",id=I(paste(ids,collapse=",")),retmax=1000,rettype="abstract"
    )
)

# Turning the output into character vector
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```

``` r
#University & Institute Case
pub_char_list <- xml2::xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)

#extracting abstracts
abstracts <- str_extract(pub_char_list, "<Abstract>[[:print:][:space:]]+</Abstract>")
abstracts <- str_remove_all(abstracts, "</?[[:alnum:]- =\"]+>")
abstracts <- str_replace_all(abstracts, "[[:space:]]+", " ")

#extracting titles
titles <- str_extract(pub_char_list, "<ArticleTitle>[[:print:][:space:]]+</ArticleTitle>")
titles <- str_remove_all(titles, "</?[[:alnum:]- =\"]+>")

#extract jounal names
journalname <- str_extract(pub_char_list, "<Title>[[:print:][:space:]]+</Title>")
journalname <- str_remove_all(journalname, "</?[[:alnum:]- =\"]+>")
journalname <- str_replace_all(journalname, "[[:space:]]+", " ")

#extract publication date
publication_date <- str_extract(pub_char_list, "<PubDate>[[:print:][:space:]]+</PubDate>")
publication_date <- str_remove_all(publication_date, "</?[[:alnum:]- =\"]+>")
publication_date <- str_replace_all(publication_date, "[[:space:]]+", " ")

database <- data.frame(
  PubMedId = ids,
  Title = titles,
  Journal_Name = journalname,
  Publication_Date = publication_date,
  Abstract = abstracts
)
knitr::kable(database[1:10,], caption="SARS-COV2 Trial Vaccine Papers")
```

| PubMedId | Title                                                                                                                                              | Journal_Name                                                                                         | Publication_Date | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|:---------|:---------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 34729549 | Adverse events of active and placebo groups in SARS-CoV-2 vaccine randomized trials: A systematic review.                                          | The Lancet regional health. Europe                                                                   | 2021 Oct 28      | For safety assessment in clinical trials, adverse events (AEs) are reported for the drug under evaluation and compared with AEs in the placebo group. Little is known about the nature of the AEs associated with clinical trials of SARS-CoV-2 vaccines and the extent to which these can be traced to nocebo effects, where negative treatment-related expectations favor their occurrence. In our systematic review, we compared the rates of solicited AEs in the active and placebo groups of SARS-CoV-2 vaccines approved by the Western pharmaceutical regulatory agencies.We implemented a search strategy to identify trial-III studies of SARS-CoV-2 vaccines through the PubMed database. We adopted the PRISMA Statement to perform the study selection and the data collection and identified three trial: two mRNA-based (37590 participants) and one adenovirus type (6736 participants). Relative risks showed that the occurrence of AEs reported in the vaccine groups was higher compared with the placebo groups. The most frequently AEs in both groups were fatigue, headache, local pain, as injection site reactions, and myalgia. In particular, for first doses in placebo recipients, fatigue was reported in 29% and 27% in BNT162b2 and mRNA-1273 groups, respectively, and in 21% of Ad26.COV2.S participants. Headache was reported in 27% in both mRNA groups and in 24% of Ad26.COV2.S recipients. Myalgia was reported in 10% and 14% in mRNA groups (BNT162b2 and mRNA-1273, respectively) and in 13% of Ad26.COV2.S participants. Local pain was reported in 12% and 17% in mRNA groups (BNT162b2 and mRNA-1273, respectively), and in 17% of Ad26.COV2.S recipients. These AEs are more common in the younger population and in the first dose of placebo recipients of the mRNA vaccines. Our results are in agreement with the expectancy theory of nocebo effects and suggest that the AEs associated with COVID-19 vaccines may be related to the nocebo effect. Fondazione CRT - Cassa di Risparmio di Torino, IT (grant number 66346, “GAIA-MENTE” 2019). © 2021 The Authors.                                                                                                                                                                                                                                                                                                                                                                                        |
| 34726743 | Analysis of the Effectiveness of the Ad26.COV2.S Adenoviral Vector Vaccine for Preventing COVID-19.                                                | JAMA network open                                                                                    | 2021 11 01       | Continuous assessment of the effectiveness and safety of the US Food and Drug Administration-authorized SARS-CoV-2 vaccines is critical to amplify transparency, build public trust, and ultimately improve overall health outcomes. To evaluate the effectiveness of the Johnson & Johnson Ad26.COV2.S vaccine for preventing SARS-CoV-2 infection. <AbstractText Label="Design, Setting, and Participants">This comparative effectiveness research study used large-scale longitudinal curation of electronic health records from the multistate Mayo Clinic Health System (Minnesota, Arizona, Florida, Wisconsin, and Iowa) to identify vaccinated and unvaccinated adults between February 27 and July 22, 2021. The unvaccinated cohort was matched on a propensity score derived from age, sex, zip code, race, ethnicity, and previous number of SARS-CoV-2 polymerase chain reaction tests. The final study cohort consisted of 8889 patients in the vaccinated group and 88 898 unvaccinated matched patients. Single dose of the Ad26.COV2.S vaccine. The incidence rate ratio of SARS-CoV-2 infection in the vaccinated vs unvaccinated control cohorts, measured by SARS-CoV-2 polymerase chain reaction testing. The study was composed of 8889 vaccinated patients (4491 men \[50.5%\]; mean \[SD\] age, 52.4 \[16.9\] years) and 88 898 unvaccinated patients (44 748 men \[50.3%\]; mean \[SD\] age, 51.7 \[16.7\] years). The incidence rate ratio of SARS-CoV-2 infection in the vaccinated vs unvaccinated control cohorts was 0.26 (95% CI, 0.20-0.34) (60 of 8889 vaccinated patients vs 2236 of 88 898 unvaccinated individuals), which corresponds to an effectiveness of 73.6% (95% CI, 65.9%-79.9%) and a 3.73-fold reduction in SARS-CoV-2 infections. This study’s findings are consistent with the clinical trial-reported efficacy of Ad26.COV2.S and the first retrospective analysis, suggesting that the vaccine is effective at reducing SARS-CoV-2 infection, even with the spread of variants such as Alpha or Delta that were not present in the original studies, and reaffirm the urgent need to continue mass vaccination efforts globally.                                                                                                                                                                                                                                                                                                                             |
| 34715931 | Lessons from Israel’s COVID-19 Green Pass program.                                                                                                 | Israel journal of health policy research                                                             | 2021 10 29       | As of the beginning of March 2021, Israeli law requires the presentation of a Green Pass as a precondition for entering certain businesses and public spheres. Entitlement for a Green Pass is granted to Israelis who have been vaccinated with two doses of COVID-19 vaccine, who have recovered from COVID-19, or who are participating in a clinical trial for vaccine development in Israel. The Green Pass is essential for retaining immune individuals’ freedom of movement and for promoting the public interest in reopening the economic, educational, and cultural spheres of activity. Nonetheless, and as the Green Pass imposes restrictions on the movement of individuals who had not been vaccinated or who had not recovered, it is not consonant with solidarity and trust building. Implementing the Green Pass provision while advancing its effectiveness on the one hand, and safeguarding equality, proportionality, and fairness on the other hand may imbue this measure with ethical legitimacy despite involving a potential breach of trust and solidarity. © 2021. The Author(s).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 34713912 | Vaccine development and technology for SARS-CoV-2: current insights.                                                                               | Journal of medical virology                                                                          | 2021 Oct 29      | SARS-CoV-2 is associated to a severe respiratory disease in China, that rapidly spread across continents. Since the beginning of the pandemic, available data suggested the asymptomatic transmission and patients were treated with specific drugs with efficacy and safety data not always satisfactory. The aim of this review is to describe the vaccines developed by three companies, Pfizer-BioNTech, Moderna and University of Oxford/AstraZeneca, in terms of both technological and pharmaceutical formulation, safety, efficacy and immunogenicity. A critical analysis of phase 1, 2 and 3 clinical trial results available was conducted, comparing the three vaccine candidates, underlining their similarities and differences. All candidates showed consistent efficacy and tolerability; although some differences can be noted, such as their technological formulation, temperature storage, which will be related to logistics and costs. Further studies will be necessary to evaluate long-term effects and to assess the vaccine safety and efficacy in the general population. This article is protected by copyright. All rights reserved. This article is protected by copyright. All rights reserved.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| 34711598 | BCG vaccination to reduce the impact of COVID-19 in healthcare workers: Protocol for a randomised controlled trial (BRACE trial).                  | BMJ open                                                                                             | 2021 10 28       | BCG vaccination modulates immune responses to unrelated pathogens. This off-target effect could reduce the impact of emerging pathogens. As a readily available, inexpensive intervention that has a well-established safety profile, BCG is a good candidate for protecting healthcare workers (HCWs) and other vulnerable groups against COVID-19. This international multicentre phase III randomised controlled trial aims to determine if BCG vaccination reduces the incidence of symptomatic and severe COVID-19 at 6 months (co-primary outcomes) compared with no BCG vaccination. We plan to randomise 10 078 HCWs from Australia, The Netherlands, Spain, the UK and Brazil in a 1:1 ratio to BCG vaccination or no BCG (control group). The participants will be followed for 1 year with questionnaires and collection of blood samples. For any episode of illness, clinical details will be collected daily, and the participant will be tested for SARS-CoV-2 infection. The secondary objectives are to determine if BCG vaccination reduces the rate, incidence, and severity of any febrile or respiratory illness (including SARS-CoV-2), as well as work absenteeism. The safety of BCG vaccination in HCWs will also be evaluated. Immunological analyses will assess changes in the immune system following vaccination, and identify factors associated with susceptibility to or protection against SARS-CoV-2 and other infections. Ethical and governance approval will be obtained from participating sites. Results will be published in peer-reviewed open-access journals. The final cleaned and locked database will be deposited in a data sharing repository archiving system. ClinicalTrials.gov NCT04327206. © Author(s) (or their employer(s)) 2021. Re-use permitted under CC BY. Published by BMJ.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| 34704204 | COVID-19 Testing and Vaccine Acceptability Among Homeless-Experienced Adults: Qualitative Data from Two Samples.                                   | Journal of general internal medicine                                                                 | 2021 Oct 26      | Homeless-experienced populations are at increased risk of exposure to SARS-CoV-2 due to their living environments and face an increased risk of severe COVID-19 disease due to underlying health conditions. Little is known about COVID-19 testing and vaccination acceptability among homeless-experienced populations. To understand the facilitators and barriers to COVID-19 testing and vaccine acceptability among homeless-experienced adults. We conducted in-depth interviews with participants from July to October 2020. We purposively recruited participants from (1) a longitudinal cohort of homeless-experienced older adults in Oakland, CA (n=37) and (2) a convenience sample of people (n=57) during a mobile outreach COVID-19 testing event in San Francisco. Adults with current or past experience of homelessness. We asked participants about their experiences with and attitudes towards COVID-19 testing and their perceptions of COVID-19 vaccinations. We used participant observation techniques to document the interactions between testing teams and those approached for testing. We audio-recorded, transcribed, and content analyzed all interviews and identified major themes and subthemes. Participants found incentivized COVID-19 testing administered in unsheltered settings and supported by community health outreach workers (CHOWs) to be acceptable. The majority of participants expressed a positive inclination toward vaccine acceptability, citing a desire to return to routine life and civic responsibility. Those who expressed hesitancy cited a desire to see trial data, concerns that vaccines included infectious materials, and mistrust of the government. Participants expressed positive evaluations of the incentivized, mobile COVID-19 testing supported by CHOWs in unsheltered settings. The majority of participants expressed a positive inclination toward vaccination. Vaccine hesitancy concerns must be addressed when designing vaccine delivery strategies that overcome access challenges. Based on the successful implementation of COVID-19 testing, we recommend mobile delivery of vaccines using trusted CHOWs to address concerns and facilitate wider access to and uptake of the COVID vaccine. © 2021. Society of General Internal Medicine.                                                                                                                                                                       |
| 34703690 | A Rare Variant of Guillain-Barre Syndrome Following Ad26.COV2.S Vaccination.                                                                       | Cureus                                                                                               | 2021 Sep         | Efforts to combat the global pandemic caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) range from adequate diagnostic testing and contract tracing to vaccination for the prevention of coronavirus disease 2019 (COVID-19). In the United States alone, three vaccinations have been authorized for emergency use (EUA) or approved to prevent COVID-19. The Ad26.COV2.S vaccine by Johnson and Johnson (New Brunswick, New Jersey) is the only adenovirus-based vaccine and deemed relatively effective and safe by the US Food and Drug Administration (FDA) following its clinical trial. Since its introduction, the US FDA has placed a warning on the vaccine adverse event reporting system (VAERS) after more than 100 cases of Guillain-Barre Syndrome (GBS) were reported. Herein, we outline the hospital course of a generally healthy 49-year-old female who experienced an axonal form of GBS nine days after receiving the Ad26.COV2.S vaccine. Copyright © 2021, Morehouse et al.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 34702753 | Humoral immunogenicity of the seasonal influenza vaccine before and after CAR-T-cell therapy: a prospective observational study.                   | Journal for immunotherapy of cancer                                                                  | 2021 10          | Recipients of chimeric antigen receptor-modified T (CAR-T) cell therapies for B cell malignancies have profound and prolonged immunodeficiencies and are at risk for serious infections, including respiratory virus infections. Vaccination may be important for infection prevention, but there are limited data on vaccine immunogenicity in this population. We conducted a prospective observational study of the humoral immunogenicity of commercially available 2019-2020 inactivated influenza vaccines in adults immediately prior to or while in durable remission after CD19-, CD20-, or B cell maturation antigen-targeted CAR-T-cell therapy, as well as controls. We tested for antibodies to all four vaccine strains using neutralization and hemagglutination inhibition (HAI) assays. Antibody responses were defined as at least fourfold titer increases from baseline. Seroprotection was defined as a HAI titer =40. Enrolled CAR-T-cell recipients were vaccinated 14-29 days prior to (n=5) or 13-57 months following therapy (n=13), and the majority had hypogammaglobulinemia and cellular immunodeficiencies prevaccination. Eight non-immunocompromised adults served as controls. Antibody responses to =1 vaccine strain occurred in 2 (40%) individuals before CAR-T-cell therapy and in 4 (31%) individuals vaccinated after CAR-T-cell therapy. An additional 1 (20%) and 6 (46%) individuals had at least twofold increases, respectively. One individual vaccinated prior to CAR-T-cell therapy maintained a response for \>3 months following therapy. Across all tested vaccine strains, seroprotection was less frequent in CAR-T-cell recipients than in controls. There was evidence of immunogenicity even among individuals with low immunoglobulin, CD19+ B cell, and CD4+ T-cell counts. These data support consideration for vaccination before and after CAR-T-cell therapy for influenza and other relevant pathogens such as SARS-CoV-2, irrespective of hypogammaglobulinemia or B cell aplasia. However, relatively impaired humoral vaccine immunogenicity indicates the need for additional infection-prevention strategies. Larger studies are needed to refine our understanding of potential correlates of vaccine immunogenicity, and durability of immune responses, in CAR-T-cell therapy recipients. © Author(s) (or their employer(s)) 2021. Re-use permitted under CC BY-NC. No commercial re-use. See rights and permissions. Published by BMJ. |
| 34698827 | Measuring vaccine efficacy against infection and disease in clinical trials: sources and magnitude of bias in COVID-19 vaccine efficacy estimates. | Clinical infectious diseases : an official publication of the Infectious Diseases Society of America | 2021 Oct 26      | Phase III trials have estimated COVID-19 vaccine efficacy (VE) against symptomatic and asymptomatic infection. We explore the direction and magnitude of potential biases in these estimates and their implications for vaccine protection against infection and against disease in breakthrough infections. We developed a mathematical model that accounts for natural and vaccine-induced immunity, changes in serostatus and imperfect sensitivity and specificity of tests for infection and antibodies. We estimated expected biases in VE against symptomatic, asymptomatic and any SARS\<U+034F>CoV2 infections and against disease following infection for a range of vaccine characteristics and measurement approaches, and the likely overall biases for published trial results that included asymptomatic infections. VE against asymptomatic infection measured by PCR or serology is expected to be low or negative for vaccines that prevent disease but not infection. VE against any infection is overestimated when asymptomatic infections are less likely to be detected than symptomatic infections and the vaccine protects against symptom development. A competing bias towards underestimation arises for estimates based on tests with imperfect specificity, especially when testing is performed frequently. Our model indicates considerable uncertainty in Oxford-AstraZeneca ChAdOx1 and Janssen Ad26.COV2.S VE against any infection, with slightly higher than published, bias-adjusted values of 59.0% (95% uncertainty interval \[UI\] 38.4 to 77.1) and 70.9% (95% UI 49.8 to 80.7) respectively. Multiple biases are likely to influence COVID-19 VE estimates, potentially explaining the observed difference between ChAdOx1 and Ad26.COV2.S vaccines. These biases should be considered when interpreting both efficacy and effectiveness study results. © The Author(s) 2021. Published by Oxford University Press for the Infectious Diseases Society of America.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 34697214 | Author Response: Guillain-Barré Syndrome in the Placebo and Active Arms of a COVID-19 Vaccine Clinical Trial.                                      | Neurology                                                                                            | 2021 10 26       | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |

SARS-COV2 Trial Vaccine Papers

# Text Mining

## Download Data

``` r
fn <- "pubmed.csv"
if (!file.exists(fn))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/03_pubmed/pubmed.csv", destfile=fn)

pubmed <- read.csv(fn)
pubmed <- as_tibble(pubmed)
```

## Tokenize the abstracts and count the number of each token. Do you see anything interesting? Does removing stop words change what tokens appear as the most frequent? What are the 5 most common tokens for each search term after removing stopwords?

``` r
pubmed %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word, sort = TRUE) %>% 
  top_n(20) %>%
  ggplot (aes(x=n, y=fct_reorder(word,n)))+
  geom_col(fill="red") +
  labs(title = "Top 20 Most Common Tokens in Abstracts with Stopwords", y= "Tokens", x="Count")
```

    ## Selecting by n

![](Assignment_3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Take out Stopwords

``` r
pubmed %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word, sort = TRUE) %>% 
  anti_join(stop_words, by ="word") %>%
  #using regex to remove numbers 
  filter(!grepl("^[0-9]+$",x=word)) %>%
  top_n(5) %>%
  ggplot (aes(x=n, y=fct_reorder(word,n)))+
  geom_col(fill="red") +
  labs(title = "Top 5 Most Common Tokens in Abstracts wihtout Stopword", y= "Tokens", x="Count")
```

    ## Selecting by n

![](Assignment_3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> From
the graph above, we see that the 5 most common tokens are covid,
patients, cancer, prostate, and disease, which is different from the
previous graph due to the removal of stop words.

``` r
pubmed %>%
  unnest_tokens(output = word, input = abstract) %>%
  anti_join(stop_words, by ="word") %>%
  group_by(term) %>%
  count(word) %>%
  top_n(5,n) %>%
  knitr::kable(caption = "The Top 5 Most Common Tokens for each search Term after Stopwords Removal")
```

| term            | word         |    n |
|:----------------|:-------------|-----:|
| covid           | 19           | 7035 |
| covid           | covid        | 7267 |
| covid           | disease      |  943 |
| covid           | pandemic     |  800 |
| covid           | patients     | 2292 |
| cystic fibrosis | cf           |  625 |
| cystic fibrosis | cystic       |  862 |
| cystic fibrosis | disease      |  400 |
| cystic fibrosis | fibrosis     |  867 |
| cystic fibrosis | patients     |  586 |
| meningitis      | clinical     |  187 |
| meningitis      | csf          |  206 |
| meningitis      | meningeal    |  219 |
| meningitis      | meningitis   |  429 |
| meningitis      | patients     |  446 |
| preeclampsia    | eclampsia    | 2005 |
| preeclampsia    | pre          | 2031 |
| preeclampsia    | preeclampsia | 1863 |
| preeclampsia    | pregnancy    |  969 |
| preeclampsia    | women        | 1196 |
| prostate cancer | cancer       | 3840 |
| prostate cancer | disease      |  652 |
| prostate cancer | patients     |  934 |
| prostate cancer | prostate     | 3832 |
| prostate cancer | treatment    |  926 |

The Top 5 Most Common Tokens for each search Term after Stopwords
Removal

From the Table above, we see that the top 5 tokens from each search term
by count. For each search term, the top 5 highest tokens by count are as
follows:<br />

**covid**: 19,covid,disease,pandemic,patients<br /> **cystic fibrosis**:
cf, cystic, disease, fibrosis, patients<br /> **meningitis**: clinical,
csf, meningeal, meningitis, patients<br /> **preeclampsia**: eclampsia,
pre,preeclampsia,pegnancy,women<br /> **prostate cancer**: cancer,
disease, patients, prostate, treatment<br />

## Tokenize the abstracts into bigrams. Find the 10 most common bigram and visualize them with ggplot2.

``` r
pubmed %>%
  unnest_ngrams(output = bigram, input = abstract, n = 2) %>%
  count(bigram, sort = TRUE) %>% 
  top_n(10) %>%
  ggplot (aes(x=n, y=fct_reorder(bigram,n)))+
  geom_col(fill="red")+
  labs(title = "Top 10 Most Common Bigrams in Abstracts with Stopwords", y= "Bigrams", x="Count")
```

    ## Selecting by n

![](Assignment_3_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> The 10
most common bigrams, as depicted in the graph above are, covid 19, of
the, in the, prostate cancer, pre eclampsie, patients with, of covid,
and the, to the, and of prostate.

## Calculate the TF-IDF value for each word-search term combination. (here you want the search term to be the “document”) What are the 5 tokens from each search term with the highest TF-IDF value? How are the results different from the answers you got in question 1?

``` r
pubmed %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word,term) %>% 
  bind_tf_idf(word,term,n) %>%
  group_by(term) %>%
  top_n(5,tf_idf) %>%
  arrange(desc(tf_idf), .by_group = TRUE) %>%
  knitr::kable(caption="Top 5 Tokens with the Highest TF-IDF Value")
```

| word            | term            |    n |        tf |       idf |    tf_idf |
|:----------------|:----------------|-----:|----------:|----------:|----------:|
| covid           | covid           | 7267 | 0.0370348 | 1.6094379 | 0.0596052 |
| pandemic        | covid           |  800 | 0.0040770 | 1.6094379 | 0.0065617 |
| coronavirus     | covid           |  647 | 0.0032973 | 1.6094379 | 0.0053068 |
| sars            | covid           |  371 | 0.0018907 | 1.6094379 | 0.0030430 |
| cov             | covid           |  333 | 0.0016971 | 1.6094379 | 0.0027313 |
| cf              | cystic fibrosis |  625 | 0.0127164 | 0.9162907 | 0.0116520 |
| fibrosis        | cystic fibrosis |  867 | 0.0176402 | 0.5108256 | 0.0090111 |
| cystic          | cystic fibrosis |  862 | 0.0175385 | 0.5108256 | 0.0089591 |
| cftr            | cystic fibrosis |   86 | 0.0017498 | 1.6094379 | 0.0028162 |
| sweat           | cystic fibrosis |   83 | 0.0016887 | 1.6094379 | 0.0027179 |
| meningitis      | meningitis      |  429 | 0.0091891 | 1.6094379 | 0.0147892 |
| meningeal       | meningitis      |  219 | 0.0046909 | 1.6094379 | 0.0075497 |
| pachymeningitis | meningitis      |  149 | 0.0031915 | 1.6094379 | 0.0051366 |
| csf             | meningitis      |  206 | 0.0044125 | 0.9162907 | 0.0040431 |
| meninges        | meningitis      |  106 | 0.0022705 | 1.6094379 | 0.0036542 |
| eclampsia       | preeclampsia    | 2005 | 0.0142711 | 1.6094379 | 0.0229684 |
| preeclampsia    | preeclampsia    | 1863 | 0.0132604 | 1.6094379 | 0.0213417 |
| pregnancy       | preeclampsia    |  969 | 0.0068971 | 0.5108256 | 0.0035232 |
| maternal        | preeclampsia    |  797 | 0.0056728 | 0.5108256 | 0.0028978 |
| gestational     | preeclampsia    |  191 | 0.0013595 | 1.6094379 | 0.0021880 |
| prostate        | prostate cancer | 3832 | 0.0311808 | 1.6094379 | 0.0501836 |
| androgen        | prostate cancer |  305 | 0.0024818 | 1.6094379 | 0.0039943 |
| psa             | prostate cancer |  282 | 0.0022946 | 1.6094379 | 0.0036931 |
| prostatectomy   | prostate cancer |  215 | 0.0017494 | 1.6094379 | 0.0028156 |
| castration      | prostate cancer |  148 | 0.0012043 | 1.6094379 | 0.0019382 |

Top 5 Tokens with the Highest TF-IDF Value

From the Table above, we see that the top 5 tokens from each search term
with the highest TF-IDF value. For each search term, the top 5 highest
tokens by TF-IDF values are as follows:<br />

**covid**: covid, pandemic, coronavirus, sars, cov<br /> **cystic
fibrosis**: cf, fibrosis, cystic, cftr, sweat<br /> **meningitis**:
meningitis, meningeal, pachymeningitis, csf, meninges<br />
**preeclampsia**: eclampsia, preeclampsia, pregnancy, maternal,
gestational<br /> **prostate cancer**: prostate, androgen, psa,
prostatectomy, castration<br />

When comparing the table from Problem 1 to this table, we see that for
each search term, they shared the following terms:

**covid**: covid, pandemic, cov<br /> **cystic fibrosis**: cf, fibrosis,
cystic<br /> **meningitis**: meningitis, meningeal, csf<br />
**preeclampsia**: eclampsia, preeclampsia, pregnancy<br /> **prostate
cancer**: prostate<br />

From this, we can see that most search terms shared at least 3 tokens
with the table from Problem 1, except for prostate cancer search term
which only shared 1 token. This is probably due to the fact that we used
a different ranking system in this table as we ordered by TF-IDF values
rather than count values.
