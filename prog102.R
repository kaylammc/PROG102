library(marinecs100b)


# Writing a utility function ----------------------------------------------

# P1: How did you extract the temperature and exposure from the hottest day?
# Copy-paste the code here.
hottest_idx <- which.max(kefj_temperature)
hottest_time <- kefj_datetime[hottest_idx]
hottest_site <- kefj_site[hottest_idx]
hotday_start <- as.POSIXct("2018-07-03 00:00", tz = "Etc/GMT+8")
hotday_end <- as.POSIXct("2018-07-03 23:59:59", tz = "Etc/GMT+8")
hotday_idx <- which(kefj_site == hottest_site &
                      kefj_datetime >= hotday_start &
                      kefj_datetime <= hotday_end)
hotday_datetime <- kefj_datetime[hotday_idx]
hotday_temperature <- kefj_temperature[hotday_idx]
hotday_exposure <- kefj_exposure[hotday_idx]
plot_kefj(hotday_datetime, hotday_temperature, hotday_exposure)
# P2: Fill in the blanks below to write the Alaskan datetime utility function.
alaskan_datetime <- function(date) {
  result <- as.POSIXct(date, tz = "Etc/GMT+8")
  return(result)
}

# Extracting data ---------------------------------------------------------

# P3: Make a copy of your code from P1 and edit it to plot the temperature and
# exposure for "Aialik" on 2012-06-01
aialikday_start <- as.POSIXct("2012-06-01 00:00", tz = "Etc/GMT+8")
aialikday_end <- as.POSIXct("2012-06-01 23:59:59", tz = "Etc/GMT+8")
aialikday_idx <- which(kefj_site == "Aialik" &
                      kefj_datetime >= aialikday_start &
                      kefj_datetime <= aialikday_end)
aialikday_datetime <- kefj_datetime[aialikday_idx]
aialikday_temperature <- kefj_temperature[aialikday_idx]
aialikday_exposure <- kefj_exposure[aialikday_idx]
plot_kefj(aialikday_datetime, aialikday_temperature, aialikday_exposure)

# P4: Make a copy of your code from P3 and edit it to plot the temperature and
# exposure for "Harris" on 2016-04-05.
harrisday_start <- as.POSIXct("2016-04-05 00:00", tz = "Etc/GMT+8")
harrisday_end <- as.POSIXct("2016-04-05 23:59:59", tz = "Etc/GMT+8")
harrisday_idx <- which(kefj_site == "Harris" &
                         kefj_datetime >= harrisday_start &
                         kefj_datetime <= harrisday_end)
harrisday_datetime <- kefj_datetime[aialikday_idx]
harrisday_temperature <- kefj_temperature[harrisday_idx]
harrisday_exposure <- kefj_exposure[harrisday_idx]
plot_kefj(harrisday_datetime, harrisday_temperature, harrisday_exposure)

#P5: Compare your solutions for P3 and P4 - what variables changed? P3 has a low
# in the early morning and peaks in the evening. P4 has two lows in the early
# morning and at night. There is a peak in the middle of the day. P3 low is
# driven by transition temp, high peak is driven by air/transition temp. For P4,
# both lows are driven by air/transition temp and the peak is driven by water
# temp.
#Changed the start time, end time, date, and plot.

# P6: What you would pick for the temperature extraction function and
# parameters' names? Harris and Aialik

# Writing extraction functions --------------------------------------------

# P7: Fill in the blanks in the code below to write your temperature extraction
# function.

extract_temp<- function(site, start, end) {
  start_alaska <- alaskan_datetime(start)
  end_alaska <- alaskan_datetime(end)
  extract_idx <- kefj_site == site &
    kefj_datetime >= start_alaska &
    kefj_datetime <= end_alaska
  result <- kefj_temperature[extract_idx]
  return(result)
}
extract_temp ("Harris","2016-04-05 00:00", "2016-04-05 23:59")


# P8: Make a copy of your solution to P7, and edit it to create exposure and
# datetime extraction functions.

extract_exposure<- function(site, start, end) {
  start_alaska <- alaskan_datetime(start)
  end_alaska <- alaskan_datetime(end)
  extract_idx <- kefj_site == site &
    kefj_datetime >= start_alaska &
    kefj_datetime <= end_alaska
  result <- kefj_exposure[extract_idx]
  return(result)
}
extract_exposure ("Harris","2016-04-05 00:00", "2016-04-05 23:59")

extract_datetime<- function(site, start, end) {
  start_alaska <- alaskan_datetime(start)
  end_alaska <- alaskan_datetime(end)
  extract_idx <- kefj_site == site &
    kefj_datetime >= start_alaska &
    kefj_datetime <= end_alaska
  result <- kefj_datetime[extract_idx]
  return(result)
}
extract_datetime ("Harris","2016-04-05 00:00", "2016-04-05 23:59")

# P9: Export your annotated screenshot as a JPEG called "annotated_function.jpg"
# and add it to your copy of the module repository. (It should be in the same
# folder as this file.)

# P10: Visualize Nuka Pass on July 1, 2018.
NP_temp <- extract_temp("Nuka_Pass", "2018-07-01 00:00", "2018-07-01 23:59")
NP_datetime <- extract_datetime("Nuka_Pass", "2018-07-01 00:00", "2018-07-01 23:59")
NP_exposure <- extract_exposure("Nuka_Pass", "2018-07-01 00:00", "2018-07-01 23:59")
plot_kefj(NP_datetime, NP_temp, NP_exposure)

# P11: Save a copy of the Nuka Pass plot as "nuka_pass_2018-07-01.png" in this
# repo

# P12: Compare the code you wrote to create the plot in this module to the code
# you wrote in PROG101. Qualitatively, how do they compare? Which one is easier
# to read and why?
# The new one is more concise and uses less lines of code so it's easier to
# read, but the one from PROG101 shows us step-by-step how to get from point A
# to point B and etc.
