#
# Generic helper functions for production analysis
#
# Author: Peeter Meos, Proekspet AS
# Date: 2. July 2017
#

# Helper function just in case
`%not in%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))
