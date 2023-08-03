```{r}
#|  label: unique-contacts-2-days
#|  echo: false
#|  warning: false

# extract unique contacts on day 1 and day 2
df_unique_cumul_contacts <- df_contact %>%
  dplyr::filter(study_day == "1" | fromdayone == "Day2 Only") %>%
  # replace NA with "unique
  dplyr::mutate(repeat_contact_d2 = case_when(is.na(repeat_contact_d2) ~ "unique",
                                              TRUE ~ repeat_contact_d2))
      
# unique contacts over 2 days
unique_num_contacts <- df_unique_cumul_contacts %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n()) %>%
  dplyr::select(rec_id, num_contacts)
df_unique_cumul_contacts <- left_join(unique_num_contacts, df_unique_cumul_contacts, by="rec_id" )
                                        
                                              
# merge participant and contact data
df_unique_cumul_contacts  <- df_unique_cumul_contacts  %>%
  filter()

# rural contacts
df_contact_rural <- df_contact %>%
  dplyr::filter(study_site == "Rural")

# urban contacts
df_contact_urban <- df_contact %>%
  dplyr::filter(study_site == "Urban")


# contacts by site
contacts_site <- df_contact %>%
  dplyr::group_by(rec_id, study_site) %>%
  dplyr::summarize(num_contacts = n(), .groups = "drop")

# median (IQR) limits
q <- c(.25, .5, .75)

median_contacts_site <- contacts_site %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(q25 = quantile(num_contacts, probs = q[1]),
                   q50 = quantile(num_contacts, probs = q[2]),
                   q75 = quantile(num_contacts, probs = q[3]),
                   .groups = "drop")

mean_contacts_site <- contacts_site %>%
  dplyr::group_by(study_site) %>%
  dplyr::summarize(mean = round(mean(num_contacts),1), 
                   .groups = "drop")


ci_contacts_rural <- contacts_site %>%
  dplyr::filter(study_site == "Rural")
ci_contacts_rural <- lm(num_contacts ~ 1, ci_contacts_rural)
ci_contacts_rural <- as.data.frame(round(confint(ci_contacts_rural), 1))
names(ci_contacts_rural)[1] <- "ll"
names(ci_contacts_rural)[2] <- "ul"

ci_contacts_urban <- contacts_site %>%
  dplyr::filter(study_site == "Urban")
ci_contacts_urban <- lm(num_contacts ~ 1, ci_contacts_urban)
ci_contacts_urban <- as.data.frame(round(confint(ci_contacts_urban), 1))
names(ci_contacts_urban)[1] <- "ll"
names(ci_contacts_urban)[2] <- "ul"

```