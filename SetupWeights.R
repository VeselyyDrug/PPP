SetupWeights <- function(w.data, ...) {
  
  # Make all species, genus, family names in lower case, strip white space at top and bottom of names
  
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  
  # Capitalise all first letters of the species, genus, and family name
  w.data <- within(w.data, {
    genus <- sapply(tolower(with(w.data, genus)), .simpleCap)
    family <- sapply(tolower(with(w.data, family)), .simpleCap)
    order <- sapply(tolower(with(w.data, order)), .simpleCap)
  })
  
  # Calculate weights for the Bs, Bg, Bf
  Bs <- aggregate(species_id ~ genus, w.data, FUN = function(x) length(unique(x)))
  Bg <- aggregate(genus ~ family, w.data, FUN = function(x) length(unique(x)))
  Bf <- aggregate(family ~ order, w.data, FUN = function(x) length(unique(x)))
  
  w.data <- merge(w.data, Bs, by = "genus")
  w.data <- merge(w.data, Bg, by = "family")
  w.data <- merge(w.data, Bf, by = "order")
  
  names(w.data) <- c("order", "family", "genus", "species_id", "species_type",
                     "sciname", "Bs.old", "Ts.old", "Bg.old", "Tg.old", "Bf.old", "Tf.old",
                     "Endem.spp", "Endem.gen", "Endem.fam", "Bs", "Bg", "Bf")
  
  # Set Tt and At variables to zero.
  w.data <- data.frame(w.data, Ts = 0, Tg = 0, Tf = 0, As = 0, Ag = 0, Af = 0, R = 0, D = 0, weight_value = 0)
  
  # Return the result
  w.data
}