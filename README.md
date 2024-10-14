# Comprehensively profiling social contact patterns in Mozambique: the GlobalMix project

*Contributors: Moses C. Kiti<sup>1</sup>, Holin Chen <sup>1</sup>, Sara Kim <sup>1</sup>*
Moses C. Kiti<sup>1,a</sup>, Charfudin Sacoor<sup>2,a</sup>, Obianuju G. Aguolu<sup>3</sup>, Alana Zelaya<sup>1</sup>, Holin Chen<sup>1</sup>, Sara S. Kim<sup>1</sup>, Nilzio Cavele<sup>2</sup>, Edgar Jamisse<sup>2</sup>, Corssino Tchavana<sup>2</sup>, Americo Jose<sup>4</sup>, Ivalda Macicame<sup>5</sup>, Orvalho Joaquim<sup>2</sup>, Noureen Ahmed<sup>6</sup>, Carol Y. Liu<sup>1</sup>, Inci Yildirim<sup>3</sup>, Kristin Nelson<sup>1</sup>, Samuel M. Jenness<sup>1</sup>, Herberth Maldonado<sup>7</sup>, Momin Kazi<sup>8</sup>, Rajan Srinivasan<sup>9</sup>, Venkata R. Mohan<sup>9</sup>, Alessia Melegaro<sup>10</sup>, Fauzia Malik<sup>6</sup>, Azucena Bardaji<sup>11</sup>, Saad B. Omer<sup>6,b</sup>, Ben Lopman<sup>1,b</sup>

*Affiliation*
<sup>1</sup> Emory University, USA
<sup>2</sup> Manhiça Health and Research Centre (CISM), Mozambique
<sup>3</sup> Yale University, USA
<sup>4</sup> Polana-Caniço Health and Research Centre (CISPOC), Mozambique
<sup>5</sup> National Institute of Health, Mozambique
<sup>6</sup> University of Texas SouthWestern Medical College, USA
<sup>7</sup> Universidad del Valle de Guatemala, Guatemala
<sup>8</sup> Aga Khan University, Pakistan
<sup>9</sup> Christian Medical College, India
<sup>10</sup> Bocconi University, Italy
<sup>11</sup> ISGlobal, Spain
<sup>a</sup> These first authors contributed equally to this article.
<sup>b</sup> These authors were co-principal investigators

*Correspondence to: blopman.emory.edu*

## Abstract

There are few sources of empirical social contact data from resource-poor settings. To alleviate this, we recruited 1363 participants from rural and urban areas of Mozambique during the COVID-19 pandemic, reporting the age, sex and relation to the contact. Participants reported a mean of 8.3 (95% CI 8.0–8.6) contacts per person. The mean contact rates were higher in the rural compared to urban site (9.8 vs 6.8, p<0.01), respectively. Using mathematical models, we report higher vaccine effects (VE) in the rural site when comparing empirical to synthetic contact matrices (32% vs 29%, respectively), and lower corresponding VE in urban site (32% vs 35%). These effects were prominent in the younger (0-9 years) and older (60+ years) individuals. Our work highlights the importance of empirical data, showing differences in contact rates and patterns between rural and urban sites in Mozambique and their non-negligible effects in modelling vaccine interventions.

## Description of repository
This contains scripts and data to generate the output for this publication. The data are arranged as follows:

1. moz_participant_data_aim1_v1.RDS - contains participant metadata. Primary key is rec_id.
2. moz_contact_data_aim1_v1.RDS - contains data on contacts. Primary key is rec_id.
3. moz_exit_interview_data_aim1.RDS - contains additional participant data. Primary key is rec_id.
4. moz_household_survey_data_aim1.RDS - contains data on households. Primary key is rec_id.
5. moz_locations_visited_data_aim1.RDS - contains data on locations visited. Primary key is rec_id.

## How to run the code
