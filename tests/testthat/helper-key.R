# API tests need a ClimMob key in the environment variable CLIMMOB_KEY.
# Never write a key in a test file: this repository is public.
climmob_key = function() Sys.getenv("CLIMMOB_KEY")

skip_without_key = function() {
  skip_on_cran()
  skip_if(climmob_key() == "", "CLIMMOB_KEY is not set")
}
