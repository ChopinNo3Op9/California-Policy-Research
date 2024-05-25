
# Example data
data(mpdta)

out1 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=NULL,
               data=mpdta)
summary(out1)

out2 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~lpop,
               data=mpdta)
summary(out2)

out3 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~lpop,
               control_group = "notyettreated",
               data=mpdta)
summary(out3)

