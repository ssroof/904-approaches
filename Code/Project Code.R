# ----------------------------------------------------
#   Data Cleaning
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
library("naniar")
library("broom")
library("foreign")
library("stargazer")
library("MASS")


pilot <- read_csv(here("data", "anes_pilot_2019.csv"))
pilot


slim_pilot <- as_tibble(pilot) %>%
  select(
    caseid,
    birthyr,
    ftjournal,
    dont_care = pop2,
    trustworthy = pop3,
    experts,
    restrict = demo4,
    science,
    exphelp,
    controlled = conspire1,
    secret = conspire2,
    lies = conspire3,
    unemp,
    interfere,
    autism = autism1,
    gmo = gmo1,
    warm,
    illegal,
    facebook = socmed_1,
    twitter = socmed_2,
    instagram = socmed_3,
    reddit = socmed_4,
    youtube = socmed_5,
    snapchat = socmed_6,
    tiktok = socmed_7,
    facebook1,
    facebook2,
    facebook3,
    twitter1,
    twitter2,
    twitter3,
    instagram1,
    instagram2,
    instagram3,
    reddit1,
    reddit2,
    reddit3,
    youtube1,
    youtube2,
    youtube3,
    snapchat1,
    snapchat2,
    snapchat3,
    tiktok1,
    tiktok2,
    tiktok3
  )



# ----------------------------------------------------
#   Codebook
# ----------------------------------------------------

# birthyr: Birth Year
  # range: 1926 - 2000

# ftjournal: How would you rate journalists?
  # feeling thermometer, 0-100

# dont_care: "Most politicians do not care about the people."
  # agree strongly (1), agree somewhat (2), neither agree nor disagree (3),
  # disagree somewhat (4), disagree strongly (5), no answer (-7)

# trustworthy: "Most politicians are trustworthy."
  # agree strongly (1), agree somewhat (2), neither agree nor disagree (3),
  # disagree somewhat (4), disagree strongly (5), no answer (-7)

# experts: "When it comes to public policy decisions, whom do you trust more,
#   ordinary people or experts?"
  # trust ordinary people much more (1), trust ordinary people somewhat more (2),
  # trust both the same (3), trust experts somewhat more (4), trust experts much more (5),
  # no answer (-7)

# restrict: "Do you favor, oppose, or neither favor nor oppose elected officials
#   restricting journalists' access to information about the policymaking process?"
  # favor a great deal (1), favor a moderate amount (2), favor a little (3),
  # neither favor nor oppose (4), oppose a little (5), oppose a moderate amount (6),
  # oppose a great deal (7), no answer (-7)

# science: "In general, how important should science be for making government decisions?"
  # not at all important (1), a little important (2), moderately important (3),
  # very important (4), extremely important (5), no answer (-7)

# exphelp: "How much do ordinary people need the help of experts to understand
#   complicated things like science and health?"
  # not at all (1), a little (2), a moderate amount (3), a lot (4), a great deal (5)

# controlled: "Most business and politics in this country are secretly controlled by
#   the same few powerful people."
  # not at all (1), not very well (2), somewhat well (3), very well (4), extremely well (5),
  # no answer (-7)

# secret: "It is usually difficult to keep a secret for long about what happens in government."
  # not at all (1), not very well (2), somewhat well (3), very well (4), extremely well (5)

# lies: "Much of what people hear in schools and the media are lies designed to keep
#   people from learning the real truth about those in power."
  # not at all (1), not very well (2), somewhat well (3), very well (4), extremely well (5),
  # no answer (-7)

# unemp: "Which of these two statements do you think is most likely to be true?"
# - Unemployment is now higher than when Donald Trump took office (1)
# - Unemployment is now lower than when Donald Trump took office (2)
# - No answer (-7)

# interfere: "Which of these two statements do you think is most likely to be true?"
# - Russia tried to interfere in the 2016 presidential election (1)
# - Russia did not try to interfere in the 2016 presidential election (2)
# - No answer (-7)

# autism: "Which of these two statements do you think is most likely to be true?"
# - Childhood vaccines cause autism
# - Childhood vaccines do not cause autism
# - Inapplicable, legitimate skip (-1)
# - No answer (-7)

# gmo: "Which of these two statements do you think is most likely to be true?"
# - Genetically modified foods are safe to eat (1)
# - Genetically modified foods are not safe to eat (2)
# - Inapplicable, legitimate skip (-1)
# - No answer (-7)

# warm: "Which of these two statements do you think is most likely to be true?"
# - World temperatures have risen on average over the last 100 years (1)
# - World temperatures have not risen on average over the last 100 years (2)
# - No answer (-7)

# illegal: "Which of these two statements do you think is most likely to be true?"
# - Millions of people voted illegally in the 2016 election (1)
# - Very few people voted illegally in the 2016 election (2)
# - No answer (-7)

# facebook, ..., tiktok
# - Visited social media platform in the past year
  # selected (1), not selected (2)

# facebook1, ..., tiktok3
# - How often do you use ____ ?
  # many times every day (1), a few times every day (2), about once a day (3),
  # a few times each week (4), about once a week (5), once or twice a month (6),
  # less than once a month (7), inapplicable, legitimate skip (-1)
# - When using ____, how often do you come across information about political issues or candidates?
  # always (1), most of the time (2), about half of the time (3), sometimes (4), never (5),
  # inapplicable, legitimate skip (-1)
# - When using ____, how often do you post information about political issues or candidates?
  # always (1), most of the time (2), about half of the time (3), sometimes (4), never (5),
  # no answer (-7), inapplicable, legitimate skip (-1)



# ----------------------------------------------------
#   Create Variables
# ----------------------------------------------------


na_strings <- c(-1, -7)


slim_pilot <- slim_pilot %>%
  replace_with_na_all(condition = ~.x %in% na_strings)


# Age Variable

slim_pilot <- slim_pilot %>%
  mutate(
    age = 2018 - birthyr
  )


# Make Social Media 0/1

slim_pilot <- slim_pilot %>%
  mutate(
    facebook = case_when(
      facebook == 2 ~ 0,
      facebook == 1 ~ 1
    ),
    twitter = case_when(
      twitter == 2 ~ 0,
      twitter == 1 ~ 1
    ),
    instagram = case_when(
      instagram == 2 ~ 0,
      instagram == 1 ~ 1
    ),
    reddit = case_when(
      reddit == 2 ~ 0,
      reddit == 1 ~ 1
    ),
    youtube = case_when(
      youtube == 2 ~ 0,
      youtube == 1 ~ 1
    ),
    snapchat = case_when(
      snapchat == 2 ~ 0,
      snapchat == 1 ~ 1
    ),
    tiktok = case_when(
      tiktok == 2 ~ 0,
      tiktok == 1 ~ 1
    )
  )


# Distrust Variable: ftjournal, dont_care, trustworthy, experts, restrict, science, exphelp
# - ftjournal: distrust (0-33), neutral (34-65), trust (66-100)
# - dont_care: distrust (1 and 2), neutral (3), trust (4 and 5)
# - trustworthy: distrust (4 and 5), neutral (3), trust (1 and 2)
# - experts: distrust (1 and 2), neutral (3), trust (4 and 5)
# - restrict: distrust (1, 2, and 3), neutral (4), trust (5, 6, and 7)
# - science: distrust (1 and 2), neutral (3), trust (4 and 5)
# - exphelp: distrust (1 and 2), neutral (3), trust (4 and 5)


slim_pilot <- slim_pilot %>%
  mutate(
    distrust_ftjournal = case_when(
      ftjournal <= 33 ~ "distrust",
      ftjournal >= 66 ~ "trust",
      TRUE ~ "neutral"
    ),
    distrust_dont_care = case_when(
      dont_care <= 2 ~ "distrust",
      dont_care == 3 ~ "neutral",
      dont_care >= 4 ~ "trust"
    ),
    distrust_trustworthy = case_when(
      trustworthy >= 4 ~ "distrust",
      trustworthy == 3 ~ "neutral",
      trustworthy <= 2 ~ "trust"
    ),
    distrust_experts = case_when(
      experts <= 2 ~ "distrust",
      experts == 3 ~ "neutral",
      experts >= 4 ~ "trust"
    ),
    distrust_restrict = case_when(
      restrict <= 3 ~ "distrust",
      restrict == 4 ~ "neutral",
      restrict >= 5 ~ "trust"
    ),
    distrust_science = case_when(
      science <= 2 ~ "distrust",
      science == 3 ~ "neutral",
      science >= 4 ~ "trust"
    ),
    distrust_exphelp = case_when(
      exphelp <= 2 ~ "distrust",
      exphelp == 3 ~ "neutral",
      exphelp >= 4 ~ "trust"
    )
  )


slim_pilot <- slim_pilot %>%
  group_by(caseid) %>%
  mutate(
    distrust_score = sum(distrust_ftjournal == "distrust",
                     distrust_dont_care == "distrust",
                     distrust_trustworthy == "distrust",
                     distrust_experts == "distrust",
                     distrust_restrict == "distrust",
                     distrust_science == "distrust",
                     distrust_exphelp == "distrust"
    ),
    trust_score = sum(distrust_ftjournal == "trust",
                               distrust_dont_care == "trust",
                               distrust_trustworthy == "trust",
                               distrust_experts == "trust",
                               distrust_restrict == "trust",
                               distrust_science == "trust",
                               distrust_exphelp == "trust"
    )
  )



# Conspiracy Variable: controlled, secret, lies
# - controlled: believe (4 and 5), neutral (3), don't believe (1 and 2)
# - secret: believe (1 and 2), neutral (3), don't believe (4 and 5)
# - lies: believe (4 and 5), neutral (3), don't believe (1 and 2)


slim_pilot <- slim_pilot %>%
  mutate(
    believe_controlled = case_when(
      controlled >= 4 ~ "believe",
      controlled == 3 ~ "neutral",
      controlled <= 2 ~ "dont"
    ),
    believe_secret =  case_when(
      secret <= 2 ~ "believe",
      secret == 3 ~ "neutral",
      secret >= 4 ~ "dont"
    ),
    believe_lies = case_when(
      lies >= 4 ~ "believe",
      lies == 3 ~ "neutral",
      lies <= 2 ~ "dont"
    )
  )


slim_pilot <- slim_pilot %>%
  group_by(caseid) %>%
  mutate(
    conspire_score = sum(believe_controlled == "believe",
                         believe_secret == "believe",
                         believe_lies == "believe"
    ),
    truth_score = sum(believe_controlled == "dont",
                      believe_secret == "dont",
                      believe_lies == "dont"
    )
  )



# Misinformation: unemp, interfere, autism, gmo, warm, illegal
# - unemp: misinformed (1), informed (2)
# - interfere: misinformed (2), informed (1)
# - autism: misinformed (1), informed (2)
# - gmo: misinformed (2), informed (1)
# - warm: misinformed (2), informed (1)
# - illegal: misinformed (1), informed (2)


slim_pilot <- slim_pilot %>%
  mutate(
    inform_unemp = case_when(
      unemp == 1 ~ "misinformed",
      unemp == 2 ~ "informed"
    ),
    inform_interfere = case_when(
      interfere == 2 ~ "misinformed",
      interfere == 1 ~ "informed"
    ),
    inform_autism = case_when(
      autism == 1 ~ "misinformed",
      autism == 2 ~ "informed"
    ),
    inform_gmo = case_when(
      gmo == 2 ~ "misinformed",
      gmo == 1 ~ "informed"
    ),
    inform_warm = case_when(
      warm == 2 ~ "misinformed",
      warm == 1 ~ "informed"
    ),
    inform_illegal = case_when(
      illegal == 1 ~ "misinformed",
      illegal == 2 ~ "informed"
    )
  )


slim_pilot <- slim_pilot %>%
  group_by(caseid) %>%
  mutate(
    misinformed_score = sum(inform_unemp == "misinformed",
                         inform_interfere == "misinformed",
                         inform_autism == "misinformed",
                         inform_gmo == "misinformed",
                         inform_warm == "misinformed",
                         inform_illegal == "misinformed"
    ),
    informed_score = sum(inform_unemp == "informed",
                         inform_interfere == "informed",
                         inform_autism == "informed",
                         inform_gmo == "informed",
                         inform_warm == "informed",
                         inform_illegal == "informed"
    )
  )



# Social Media Variables

# Activity (how active users are):
# - Active politically engaged user: uses often and posts political content
# selected (1-4) in facebook1, ..., tiktok1 *AND*
# selected (1-3) in facebook3, ..., tiktok3
# - Active politically disengaged user: uses often and does not post political content
# selected (1-4) in facebook1, ..., tiktok1 *AND*
# selected (4 and 5) in facebook3, ..., tiktok3
# - Inactive user: does not use often
# selected (5-7) in facebook1, ..., tiktok1
# - Not a user:
# selected (2) in facebook, ..., tiktok


# Activity

slim_pilot <- slim_pilot %>%
  mutate(
    fb_activity = case_when(
      facebook1 <= 4 ~ "active",
      facebook1 >= 5 ~ "inactive"
    ),
    twit_activity = case_when(
      twitter1 <= 4 ~ "active",
      twitter1 >= 5 ~ "inactive"
    ),
    ig_activity = case_when(
      instagram1 <= 4 ~ "active",
      instagram1 >= 5 ~ "inactive"
    ),
    red_activity = case_when(
      reddit1 <= 4 ~ "active",
      reddit1 >= 5 ~ "inactive"
    ),
    yout_activity = case_when(
      youtube1 <= 4 ~ "active",
      youtube1 >= 5 ~ "inactive"
    ),
    sc_activity = case_when(
      snapchat1 <= 4 ~ "active",
      snapchat1 >= 5 ~ "inactive"
    ),
    tt_activity = case_when(
      tiktok1 <= 4 ~ "active",
      tiktok1 >= 5 ~ "inactive"
    )
  )


# Engagement

slim_pilot <- slim_pilot %>%
  mutate(
    fb_engage = case_when(
      facebook3 <= 3 ~ "engaged",
      facebook3 >= 4 ~ "unengaged"
    ),
    twit_engage = case_when(
      twitter3 <= 3 ~ "engaged",
      twitter3 >= 4 ~ "unengaged"
    ),
    ig_engage = case_when(
      instagram3 <= 3 ~ "engaged",
      instagram3 >= 4 ~ "unengaged"
    ),
    red_engage = case_when(
      reddit3 <= 3 ~ "engaged",
      reddit3 >= 4 ~ "unengaged"
    ),
    yout_engage = case_when(
      youtube3 <= 3 ~ "engaged",
      youtube3 >= 4 ~ "unengaged"
    ),
    sc_engage = case_when(
      snapchat3 <= 3 ~ "engaged",
      snapchat3 >= 4 ~ "unengaged"
    ),
    tt_engage = case_when(
      tiktok3 <= 3 ~ "engaged",
      tiktok3 >= 4 ~ "unengaged"
    )
  )


# Active engaged: act_eng
# Active unengaged: act_uneng
# Inactive: inact_user
# Not online: offline

slim_pilot <- slim_pilot %>%
  group_by(caseid) %>%
  mutate(
    user_type = case_when(
      (fb_activity == "active" |
         twit_activity == "active" |
         ig_activity == "active" |
         red_activity == "active" |
         yout_activity == "active" |
         sc_activity == "active" |
         tt_activity == "active") &
      (fb_engage == "engaged" |
         twit_engage == "engaged" |
         ig_engage == "engaged" |
         red_engage == "engaged" |
         yout_engage == "engaged" |
         sc_engage == "engaged" |
         tt_engage == "engaged") ~ "act_eng",
      (fb_activity == "active" |
         twit_activity == "active" |
         ig_activity == "active" |
         red_activity == "active" |
         yout_activity == "active" |
         sc_activity == "active" |
         tt_activity == "active") &
      (fb_engage == "unengaged" |
         twit_engage == "unengaged" |
         ig_engage == "unengaged" |
         red_engage == "unengaged" |
         yout_engage == "unengaged" |
         sc_engage == "unengaged" |
         tt_engage == "unengaged") ~ "act_uneng",
      (fb_activity == "inactive" |
         twit_activity == "inactive" |
         ig_activity == "inactive" |
         red_activity == "inactive" |
         yout_activity == "inactive" |
         sc_activity == "inactive" |
         tt_activity == "inactive") ~ "inact_user",
      (facebook == 0 &
         twitter == 0 &
         instagram == 0 &
         reddit == 0 &
         youtube == 0 &
         snapchat == 0 &
         tiktok == 0) ~ "offline"
    )
  )



# ----------------------------------------------------
#   Final Dataset
# ----------------------------------------------------


final <- as_tibble(slim_pilot) %>%
  select(
    caseid,
    age,
    facebook,
    twitter,
    instagram,
    reddit,
    youtube,
    snapchat,
    tiktok,
    facebook2,
    twitter2,
    instagram2,
    reddit2,
    youtube2,
    snapchat2,
    tiktok2,
    distrust_score,
    trust_score,
    conspire_score,
    truth_score,
    misinformed_score,
    informed_score,
    fb_activity,
    twit_activity,
    ig_activity,
    red_activity,
    yout_activity,
    sc_activity,
    tt_activity,
    fb_engage,
    twit_engage,
    ig_engage,
    red_engage,
    yout_engage,
    sc_engage,
    tt_engage,
    user_type
  )


# Toxicity Score

final <- final %>%
  group_by(caseid) %>%
  mutate(
    toxicity_score = sum(distrust_score, conspire_score, misinformed_score, na.rm = TRUE)
  )


# Non-Toxic Score

final <- final %>%
  group_by(caseid) %>%
  mutate(
    non_score = sum(trust_score, truth_score, informed_score, na.rm = TRUE)
  )



# ----------------------------------------------------
#   Descriptive Work
# ----------------------------------------------------

# Distrust Score Distribution

ggplot(final) +
  aes(x = distrust_score, na.rm = TRUE) +
  geom_histogram(
    binwidth = .5, na.rm = TRUE) +
  geom_vline(aes(xintercept = median(final$distrust_score, na.rm = TRUE), color = "Median")) +
  geom_vline(aes(xintercept = mean(final$distrust_score, na.rm = TRUE), color = "Mean")) +
  theme(legend.title = element_blank()) +
  xlab("Distrust Score") + 
  ylab("Count") +
  ggtitle("Distrust Distribution") +
  theme(plot.title = element_text(hjust = 0.5))


# Conspiracy Score Distribution

ggplot(final) +
  aes(x = conspire_score, na.rm = TRUE) +
  geom_histogram(
    binwidth = .5, na.rm = TRUE) +
  geom_vline(aes(xintercept = median(final$conspire_score, na.rm = TRUE), color = "Median")) +
  geom_vline(aes(xintercept = mean(final$conspire_score, na.rm = TRUE), color = "Mean")) +
  theme(legend.title = element_blank()) +
  xlab("Conspiracy Score") + 
  ylab("Count") +
  ggtitle("Conspiracy Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Misinformed Score Distribution

ggplot(final) +
  aes(x = misinformed_score, na.rm = TRUE) +
  geom_histogram(
    binwidth = .5, na.rm = TRUE) +
  geom_vline(aes(xintercept = median(final$misinformed_score, na.rm = TRUE), color = "Median")) +
  geom_vline(aes(xintercept = mean(final$misinformed_score, na.rm = TRUE), color = "Mean")) +
  theme(legend.title = element_blank()) +
  xlab("Misinformed Score") + 
  ylab("Count") +
  ggtitle("Misinformed Distribution") +
  theme(plot.title = element_text(hjust = 0.5))


# Toxicity Score Distribution

ggplot(final) +
  aes(x = toxicity_score, na.rm = TRUE) +
  geom_histogram(
    binwidth = .5, na.rm = TRUE) +
  geom_vline(aes(xintercept = median(final$toxicity_score, na.rm = TRUE), color = "Median")) +
  geom_vline(aes(xintercept = mean(final$toxicity_score, na.rm = TRUE), color = "Mean")) +
  theme(legend.title = element_blank()) +
  xlab("Toxicity Score") + 
  ylab("Count") +
  ggtitle("Toxicity Distribution") +
  theme(plot.title = element_text(hjust = 0.5))


# Non-Toxic Score Distribution

ggplot(final) +
  aes(x = non_score, na.rm = TRUE) +
  geom_histogram(
    binwidth = .5, na.rm = TRUE) +
  geom_vline(aes(xintercept = median(final$non_score, na.rm = TRUE), color = "Median")) +
  geom_vline(aes(xintercept = mean(final$non_score, na.rm = TRUE), color = "Mean")) +
  theme(legend.title = element_blank()) +
  xlab("Non-Toxic Score") + 
  ylab("Count") +
  ggtitle("Non-Toxic Score Distribution") +
  theme(plot.title = element_text(hjust = 0.5))


# User Type Distribution

ggplot(final, aes(x = user_type)) +
  geom_bar(stat = "count", na.rm = TRUE) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set1") +
  xlab("User Type") + 
  ylab("Count") +
  ggtitle("User Type Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(
    limits = c("act_uneng", "act_eng", "inact_user", "offline"),
    labels = c("act_eng" = "Active Engaged", "act_uneng" = "Active Unengaged",
               "inact_user" = "Inactive User", "offline" = "Offline"))


# Age Distribution

ggplot(final) +
  aes(x = age, na.rm = TRUE) +
  geom_histogram(
    binwidth = .5, na.rm = TRUE) +
  geom_vline(aes(xintercept = median(final$age, na.rm = TRUE), color = "Median")) +
  geom_vline(aes(xintercept = mean(final$age, na.rm = TRUE), color = "Mean")) +
  theme(legend.title = element_blank()) +
  xlab("Age") + 
  ylab("Count") +
  ggtitle("Age Distribution") +
  theme(plot.title = element_text(hjust = 0.5))


# Age and Media Platforms - didn't use in paper

ggplot(final, aes(y = age)) +
  geom_smooth(method = "lm", se = FALSE, aes(x = facebook, color = "Facebook")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = twitter, color = "Twitter")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = instagram, color = "Instagram")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = reddit, color = "Reddit")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = youtube, color = "YouTube")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = snapchat, color = "Snapchat")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = tiktok, color = "TikTok")) +
  labs(
    x = "Media Use",
    y = "Age",
    colour = "Platform",
    title = "Toxicity Comparison between Platform Users and Non-Users"
  )


# Toxicity Score by Platform

final %>%
  group_by(facebook) %>%
  filter(facebook == 1) %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(twitter) %>%
  filter(twitter == 1) %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(instagram) %>%
  filter(instagram == 1) %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(reddit) %>%
  filter(reddit == 1) %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(youtube) %>%
  filter(youtube == 1) %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(snapchat) %>%
  filter(snapchat == 1) %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(tiktok) %>%
  filter(tiktok == 1) %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

dat1 <- data.frame(
  Platform = c("Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok"),
  Toxic = c(4.59, 4.23, 4.23, 3.86, 4.48, 4.05, 4.06)
)

ggplot(dat1)+
  aes(x = Platform, y = Toxic) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Toxic), position = position_dodge(width = 0.9), vjust = -2) +
  ylim(0, 16) +
  geom_hline(yintercept = mean(dat1$Toxic), color = "red", linetype = "dashed") +
  xlab("Platform Name") + 
  ylab("Toxicity Score") +
  ggtitle("Toxicity Scores by Social Media Platform") +
  theme(plot.title = element_text(hjust = 0.5))


# Non-Toxic Score by Platform

final %>%
  group_by(facebook) %>%
  filter(facebook == 1) %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(twitter) %>%
  filter(twitter == 1) %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(instagram) %>%
  filter(instagram == 1) %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(reddit) %>%
  filter(reddit == 1) %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(youtube) %>%
  filter(youtube == 1) %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(snapchat) %>%
  filter(snapchat == 1) %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(tiktok) %>%
  filter(tiktok == 1) %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

dat2 <- data.frame(
  Platform = c("Facebook", "Twitter", "Instagram", "Reddit", "YouTube", "Snapchat", "TikTok"),
  Non = c(5.19, 5.80, 5.54, 6.49, 5.44, 5.28, 5.90)
)

ggplot(dat2)+
  aes(x = Platform, y = Non) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Non), position = position_dodge(width = 0.9), vjust = -2) +
  ylim(0, 16) +
  geom_hline(yintercept = mean(dat2$Non), color = "red", linetype = "dashed") +
  xlab("Platform Name") + 
  ylab("Non-Toxic Score") +
  ggtitle("Non-Toxic Scores by Social Media Platform") +
  theme(plot.title = element_text(hjust = 0.5))


# Toxicity Score by User Type

final %>%
  group_by(user_type) %>%
  filter(user_type == "act_eng") %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(user_type) %>%
  filter(user_type == "act_uneng") %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(user_type) %>%
  filter(user_type == "inact_user") %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

final %>%
  group_by(user_type) %>%
  filter(user_type == "offline") %>%
  summarize(
    mean = mean(toxicity_score, na.rm = TRUE)
  )

dat3 <- data.frame(
  User = c("Active Engaged", "Active Unengaged", "Inactive User", "Offline"),
  Toxic = c(4.88, 4.41, 4.90, 4.83)
)

ggplot(dat3)+
  aes(x = User, y = Toxic) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Toxic), position = position_dodge(width = 0.9), vjust = -2) +
  ylim(0, 16) +
  geom_hline(yintercept = mean(dat3$Toxic), color = "red", linetype = "dashed") +
  xlab("User Type") + 
  ylab("Toxicity Score") +
  ggtitle("Toxicity Scores by User Type") +
  theme(plot.title = element_text(hjust = 0.5))


# Non-Toxic Score by User Type

final %>%
  group_by(user_type) %>%
  filter(user_type == "act_eng") %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(user_type) %>%
  filter(user_type == "act_uneng") %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(user_type) %>%
  filter(user_type == "inact_user") %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

final %>%
  group_by(user_type) %>%
  filter(user_type == "offline") %>%
  summarize(
    mean = mean(non_score, na.rm = TRUE)
  )

dat4 <- data.frame(
  User = c("Active Engaged", "Active Unengaged", "Inactive User", "Offline"),
  Non = c(5.12, 5.25, 5.43, 4.43)
)

ggplot(dat4)+
  aes(x = User, y = Non) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Non), position = position_dodge(width = 0.9), vjust = -2) +
  ylim(0, 16) +
  geom_hline(yintercept = mean(dat4$Non), color = "red", linetype = "dashed") +
  xlab("User Type") + 
  ylab("Non-Toxic Score") +
  ggtitle("Non-Toxic Scores by User Type") +
  theme(plot.title = element_text(hjust = 0.5))



# ----------------------------------------------------
#   Model Analysis
# ----------------------------------------------------


# Toxicity by Platform USe

ggplot(final, aes(y = toxicity_score)) +
  geom_smooth(method = "lm", se = FALSE, aes(x = facebook, color = "Facebook")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = twitter, color = "Twitter")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = instagram, color = "Instagram")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = reddit, color = "Reddit")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = youtube, color = "YouTube")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = snapchat, color = "Snapchat")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = tiktok, color = "TikTok")) +
  labs(
    x = "Media Use",
    y = "Toxicity Score",
    colour = "Platform",
    title = "Toxicity Comparison between Platform Users and Non-Users"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# Non-Toxic by Platform Use

ggplot(final, aes(y = non_score)) +
  geom_smooth(method = "lm", se = FALSE, aes(x = facebook, color = "Facebook")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = twitter, color = "Twitter")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = instagram, color = "Instagram")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = reddit, color = "Reddit")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = youtube, color = "YouTube")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = snapchat, color = "Snapchat")) +
  geom_smooth(method = "lm", se = FALSE, aes(x = tiktok, color = "TikTok")) +
  labs(
    x = "Media Use",
    y = "Non-Toxic Score",
    colour = "Platform",
    title = "Non-Toxic Score Comparison between \n Platform Users and Non-Users"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# Complete Models: Toxicity and Non-Toxic

fb_tox <- lm(toxicity_score ~ facebook + age + as.factor(user_type), data = final)
fb_non <- lm(non_score ~ facebook + age + as.factor(user_type), data = final)

ig_tox <- lm(toxicity_score ~ instagram + age + as.factor(user_type), data = final)
ig_non <- lm(non_score ~ instagram + age + as.factor(user_type), data = final)

twit_tox <- lm(toxicity_score ~ twitter + age + as.factor(user_type), data = final)
twit_non <- lm(non_score ~ twitter + age + as.factor(user_type), data = final)

red_tox <- lm(toxicity_score ~ reddit + age + as.factor(user_type), data = final)
red_non <- lm(non_score ~ reddit + age + as.factor(user_type), data = final)

you_tox <- lm(toxicity_score ~ youtube + age + as.factor(user_type), data = final)
you_non <- lm(non_score ~ youtube + age + as.factor(user_type), data = final)

sc_tox <- lm(toxicity_score ~ snapchat + age + as.factor(user_type), data = final)
sc_non <- lm(non_score ~ snapchat + age + as.factor(user_type), data = final)

tt_tox <- lm(toxicity_score ~ tiktok + age + as.factor(user_type), data = final)
tt_non <- lm(non_score ~ tiktok + age + as.factor(user_type), data = final)


stacked_tox <- 
  bind_rows(
    "Facebook" = tidy(fb_tox, conf.int = TRUE),
    "Twitter" = tidy(twit_tox, conf.int = TRUE),
    "Instagram" = tidy(ig_tox, conf.int = TRUE),
    "Reddit" = tidy(red_tox, conf.int = TRUE),
    "YouTube" = tidy(you_tox, conf.int = TRUE),
    "Snapchat" = tidy(sc_tox, conf.int = TRUE),
    "TikTok" = tidy(tt_tox, conf.int = TRUE),
    .id = "Platform"
  )

stacked_non <- 
  bind_rows(
    "Facebook" = tidy(fb_non, conf.int = TRUE),
    "Twitter" = tidy(twit_non, conf.int = TRUE),
    "Instagram" = tidy(ig_non, conf.int = TRUE),
    "Reddit" = tidy(red_non, conf.int = TRUE),
    "YouTube" = tidy(you_non, conf.int = TRUE),
    "Snapchat" = tidy(sc_non, conf.int = TRUE),
    "TikTok" = tidy(tt_non, conf.int = TRUE),
    .id = "Platform"
  )


ggplot(stacked_tox) +
  aes(x = term, y = estimate, color = Platform) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.5)
  ) +
  coord_flip() +
  labs(
    x = "Term",
    y = "Estimate",
    title = "Platform Toxicity Coefficients"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(stacked_non) +
  aes(x = term, y = estimate, color = Platform) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.5)
  ) +
  coord_flip() +
  labs(
    x = "Term",
    y = "Estimate",
    title = "Platform Non-Toxic Score Coefficients"
  ) +
  theme(plot.title = element_text(hjust = 0.5))



stargazer(fb_tox, twit_tox, ig_tox, red_tox, you_tox, sc_tox, tt_tox,
          dep.var.labels = "Platform Model",
          covariate.labels = c("YouTube", "Twitter", "TikTok", "Snapchat", "Reddit", "Instagram",
                               "Facebook", "Offline", "Inactive User", "Active Unengaged", "Age",
                               "Intercept"),
          no.space = TRUE, type = "html", out = "tox.htm")


stargazer(fb_non, twit_non, ig_non, red_non, you_non, sc_non, tt_non,
          dep.var.labels = "Platform Model",
          covariate.labels = c("YouTube", "Twitter", "TikTok", "Snapchat", "Reddit", "Instagram",
                               "Facebook", "Offline", "Inactive User", "Active Unengaged", "Age",
                               "Intercept"),
          no.space = TRUE, type = "html", out = "non.htm")


