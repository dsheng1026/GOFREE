devtools::load_all(".")
prj <- ReadGCAM(filetype = 'prj',
                input_path = system.file("extdata", package = "GOFREE"),
                prj_name = "package.dat")

EJ_activity <- GCAM_EJ(prj) # ~ 20s
GW_activity <- GCAM_GW(EJ_activity) # ~ 4s
JOB_activity <- GCAM_JOB(GW_activity, "Net")

PLOT_EF()
PLOT_GW(GW_activity, "Net")
PLOT_JOB(JOB_activity)
PLOT_JOB_TYPE(JOB_activity)
MAP_JOB(JOB_activity, 2050)
