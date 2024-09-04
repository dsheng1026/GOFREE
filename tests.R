prj <- ReadGCAM(filetype = 'prj',
                input_path = "C:/GODEEP",
                prj_name = "package.dat")

EJ_activity <- GCAM_EJ(prj)
GW_activity <- GCAM_GW(EJ_activity)
JOB_activity <- GCAM_JOB(GW_activity, "Net")

PLOT_EF()
PLOT_GW(GW_activity, "Net")
PLOT_JOB(JOB_activity)
PLOT_JOB_TYPE(JOB_activity)
MAP_JOB(JOB_activity, 2050)
