devtools::load_all(".")
prj <- ReadGCAM(filetype = 'prj',
                input_path = system.file("extdata", package = "GCAMUSAJobs"),
                prj_name = "package.dat")

prj <- ReadGCAM(filetype = 'prj',
                        input_path = "C:/GODEEPPackage/GCAMUSAJobs/",
                        prj_name = "package.dat")

prj <- ReadGCAM(filetype = 'prj',
                        input_path = "C:/GODEEP/",
                        prj_name = "package.dat")

prj <- ReadGCAM(filetype = 'db',
                        input_path = "C:/GODEEP/GCAM7/output",
                        db_name = "database_basexdbGCAM-USA_Ref",
                        scen_name = "GCAM-USA_Ref", # this needs to match the scenario name specified in the DB
                        prj_name = "mydata.dat") # saved in working directory

EJ_activity <- GCAM_EJ(prj) # ~ 20s
GW_activity <- GCAM_GW(EJ_activity) # ~ 4s

JOB_activity1 <- GCAM_JOB(GW_activity, "total")
JOB_activity2 <- GCAM_JOB(GW_activity, "Total")
JOB_activity <- GCAM_JOB(GW_activity) # expect these three to return the same outcomes
identical(JOB_activity1, JOB_activity2)
identical(JOB_activity, JOB_activity2)
JOB_activity3 <- GCAM_JOB(GW_activity, "Net")
JOB_activity4 <- GCAM_JOB(GW_activity, "123") # expect to report error

PLOT_EF()
PLOT_EF("AB") # expect error message
PLOT_EF("CO")
PLOT_GW(GW_activity)
PLOT_GW(GW_activity, "AB") # expect error message
PLOT_GW(GW_activity, "TX")
PLOT_GW(GW_activity, "TX" ,"net")
PLOT_GW(GW_activity, "TX", "Total")
PLOT_JOB(JOB_activity)
PLOT_JOB(JOB_activity, "CO")
PLOT_JOB(JOB_activity, "AB") # expect error message
PLOT_JOB_TYPE(JOB_activity)
PLOT_JOB_TYPE(JOB_activity, "AB") # expect error message
PLOT_JOB_TYPE(JOB_activity, "CO")
MAP_JOB(JOB_activity, 2050)

