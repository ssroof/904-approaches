# ----------------------------------------------------
#   Data Cleaning
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")


pilot <- read_csv(here("data", "anes_pilot_2019.csv"))
pilot

slim_pilot <- pilot %>%
  select(
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
    party_id = pid1d,
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

# party_id: "Generally speaking, do you usually think of yourself as a Democrat,
#   a Republican, an independent, or what?"
  # Democrat (1), Republican (2), Independent (3), Something else (4), No answer (-7),
  # Inapplicable, legisimate skip (-1)

# facebook, ..., tiktok
# - Visited social media platform in the past year
  # selected (1), not selected (2)

# facebook1, ..., tiktok3
# - How often do you use ____ ?
  # many times every day (1), a few times every day (2), about once a day (3),
  # a few times each week (4), about once a week (5), once or twice a month (6),
  # less than once a month (7), incapplicable, legitimate skip (-1)
# - When using ____, how often do you come across information about political issues or candidates?
  # always (1), most of the time (2), about half of the time (3), sometimes (4), never (5),
  # inapplicable, legitimate skip (-1)
# - When using ____, how often do you post information about political issues or candidates?
  # always (1), most of the time (2), about half of the time (3), sometimes (4), never (5),
  # no answer (-7), inapplicable, legitimate skip (-1)



