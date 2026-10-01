# Smoke-check the translators without launching Shiny.
# Usage: Rscript tools/self_check.R

args_ok <- file.exists("R/translate.R")
if (!args_ok) {
  if (file.exists("../R/translate.R")) setwd("..")
}
source("R/translate.R")

sas <- "
data adults;
  set patients;
  where age ge 18;
  bmi = weight / (height ** 2);
  keep subject age bmi;
run;
"
py <- sas_to_python(sas)
cat("----- SAS to Python -----\n")
cat(py$code, "\n")
cat(paste("-", py$notes, collapse = "\n"), "\n")

back <- python_to_sas('adults = patients.query("age >= 18")')
cat("----- Python to SAS -----\n")
cat(back$code, "\n")
stopifnot(nzchar(py$code), nzchar(back$code))
cat("self-check ok\n")
