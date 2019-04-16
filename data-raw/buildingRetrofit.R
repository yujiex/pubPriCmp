library("readxl")
library("dplyr")

readxl::read_excel("retrofit_record_correct_version/Light-Touch M&V - ARRA Targets to Actuals and Commissioning Details.xlsx", sheet=2, skip=3) %>%
  dplyr::select(`Building ID`, `Project Name`, `Total ARRA Obligation`, `ARRA Substantial Completion Date`,
                `Advanced Metering`, `Building Envelope`, `Building Tune Up`, `HVAC`, `Indoor Environmental Quality`,
                `Lighting`) %>%
  head()
