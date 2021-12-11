# Associatation Analysis bewteen Cholesterol and Heart Disease Patients
# Author
Ram Ayyala
11/10/2021
# Introduction

Heart Disease is a major problem people across the world, whether it be from minor heart problems all the way to life threatening heart conditions. Many heart diseases can be congenital, but many are also brought on by our lifestyle, such as eating habits. Cholesterol, is a substance found in nearly every cell of our body, but also found in many foods we ingest like meat and dairy products. Our body needs cholesterol in order to function correctly, but as saying goes "too much of something, is sometimes not good for you!" Too much cholesterol, or high cholesterol can lead individuals to have a higher risk of heart disease in the arteries. In this study, we will be using the UCI Heart Disease Dataset in order to determine whether patients suffering from heart disease due to the narrowing of the diameter of the arteries is associated with high cholesterol. 

## Data Set Information
The data set in this study comes from the UCI Heart Disease Dataset, which contains samples from three specific areas around the world: Cleveland, Ohio in the USA, Long Beach, California in the USA, Zurich and Basel in Switzerland and finally Budapest, Hungary. Note that while the Switzerland dataset is processed in this analysis, it is essentially removed from the merged dataset as they did not have any cholesterol records in their dataset. This data set contains 14 different attributes, of which information is provided about each column below:<br /> 
    **age:** Age in years<br /> 
    **sex:** (1 = male; 0 = female)<br /> 
    **cp:** Chest pain type (0 = asymptomatic; 1 = atypical angina; 2 = non-anginal pain; 3 = typical angina) <br />
    **trestbps:** Resting blood pressure (in mm Hg on admission to the hospital) <br />
    **cholserum:** Cholestoral in mg/dl <br />
    **fbs:** Fasting blood sugar > 120 mg/dl (1 = true; 0 = false) <br />
    **restecg:** Resting electrocardiographic results (0= showing probable or definite left ventricular hypertrophy by Estes' criteria; 1 = normal; 2 = having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)) <br />
    **thalach:** Maximum heart rate achieved <br />
    **exang:** Exercise induced angina (1 = yes; 0 = no) <br />
    **oldpeakST:** Depression induced by exercise relative to rest <br />
    **slope:** The slope of the peak exercise ST segment (0 = downsloping; 1 = flat; 2 = upsloping) <br />
    **ca:** Number of major vessels (0-4) colored by flourosopy <br />
    **thal:** 1 = normal; 2 = fixed defect; 3 = reversable defect <br />
    **num:**  num: diagnosis of heart disease (angiographic disease status) <br />
        -- Value 0: < 50% diameter narrowing <br />
        -- Value 1: > 50% diameter narrowing <br />
        (in any major vessel: attributes 59 through 68 are vessels) <br />

# Questions to be Addressed:
For this study, we are mainly interested in **whether high cholesterol is associated with the narrowing of the diameter of the arteries in heart disease patient**. 

Our sub priorities in the study is to see whether: <br />

  **1. Do certain locations have a higher instance of cholesterol levels in their patient samples?** <br />
  
  **2. Do certain locations have a higher instance of the narrowing of the diameter of the arteries in their patient samples when compared to others?** <br />
  
  **3. Is there an association between resting blood pressure and cholesterol levels?**<br />

# To see the full study, please click the link below
[Study](https://ramayyala.github.io/PM566-Labs/)<br />
[PDF Report] (https://github.com/ramayyala/PM566-Labs/tree/master/final_project/_site/Report.pdf)<br />
# Code
[Availabe Here](https://github.com/ramayyala/PM566-Labs/blob/master/final_project/final_project.Rmd)
