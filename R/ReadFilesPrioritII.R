
cat("Reading data...",'\n')

# benefit data
b.names <- c("species_id", "taxa_text", "taxa_code", "spp_text",
             "sciname", "bene_with_action", "bene_no_action",
             "spp_code")

b.classes <- c("integer", "character", "integer", "character",
               "character", "integer", "integer", "integer")

b.data <- read.table(file = file.path(directory, "BenefitData.csv"), header = TRUE, sep = ",",
                     col.names = b.names, colClasses = b.classes, quote = "\"", comment.char = "")

# action data
a.names <-  c("action_id", "species_id",
              "taxa_code_text", "taxa_code", "spp_text", "spp_code",
              "method", "method1_text", "method2_text", "method3_text",
              "method4_text", "period1_cost", "period1_sequence",
              "period2_cost", "period2_sequence",
              "period3_cost", "period3_sequence", "pr_t_data",
              "pr_o_data", "pr_m_data")

a.classes <- c("integer", "integer",
               "character", "integer", "character", "integer",
               "character", "character", "character", "character",
               "character", "numeric", "character",
               "numeric", "character",
               "numeric",
               "character", "integer","integer", "integer")

a.data <- read.table(file = file.path(directory, "ActionData.csv"), header = TRUE, sep = ",",
                     col.names = a.names, colClasses = a.classes, quote = "\"", comment.char = "")

# weights data
w.data <- read.table(file = file.path(directory,"WeightsData.csv"), header = TRUE, sep = ",",
                     quote = "\"", comment.char = "", strip.white = TRUE)

# overlap data
o.data <- read.table(file = file.path(directory,"OverlapData.csv"), header = TRUE, sep = ",",
                     quote = "\"", comment.char = "", strip.white = TRUE)

# cost period data 
cost.period1<- as.matrix(read.table(file = file.path(directory, "Period1.csv"), header = TRUE, sep = ",",
                          quote = "\"", comment.char = "", strip.white = TRUE))

cost.period2<- as.matrix(read.table(file = file.path(directory, "Period2.csv"), header = TRUE, sep = ",",
                          quote = "\"", comment.char = "", strip.white = TRUE))
                          
cost.period3<- as.matrix(read.table(file = file.path(directory, "Period3.csv"), header = TRUE, sep = ",",
                          quote = "\"", comment.char = "", strip.white = TRUE))

output_PPP<-PrioritisationII(directory, weighting = 1, 
                             multiplier = 1, set_budget=1000000000, overlap.indicator, 
                             discount.rate = 0.01, T = 50, stop.iteration = Inf, 
                             a.data, b.data, o.data, w.data, cost.period1, 
                             cost.period2, cost.period3)
