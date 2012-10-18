UpdateWeightsII <- 
function(weights.data, just.removed, remaining, ...){
        
        if(length(just.removed)==0)
            return(list(w = weights.data, aff = NULL))

        # PLAN:
        # 1.) Update Ts, As, Tg, Ag, Tf, Af in weights.data from removed list.  
        # 2.) Find the affected vector.  
        # 3.) Intersect the affected vector and the remaining vector.  

        # 1.) Update the Ts values of species in just.removed (switch to 1)
        to.update.spp <- match(just.removed, with(weights.data, species_id))
        #to.update.spp <- match(just.removed, species_id)
        weights.data[to.update.spp, "Ts"] <- 1
        #Ts[to.update.spp] <- 1

        # Find all species in the same GENUS as those in 'just.removed' 
        to.update.gen <- subset(weights.data[to.update.spp,], select = genus)
        to.update.gen <- unique(unlist(to.update.gen))
        to.update.gen <- with(weights.data, genus) %in% to.update.gen
        #to.update.gen <- genus[to.update.spp,]
        #to.update.gen <- unique(unlist(to.update.gen))
        #to.update.gen <- genus %in% to.update.gen
        
        # Find all species in the same FAMILY as those in 'just.removed'.  
        to.update.fam <- subset(weights.data[to.update.spp,], select = family)
        to.update.fam <- unique(unlist(to.update.fam))
        to.update.fam <- with(weights.data, family) %in% to.update.fam
        
        # Find all species in the same ORDER as those removed.  
        to.update.ord <- subset(weights.data[to.update.spp,], select = order)
        to.update.ord <- unique(unlist(to.update.ord))
        to.update.ord <- with(weights.data, order) %in% to.update.ord

        # Calculate As and merge with weights.data
        As <- aggregate(Ts ~ genus, weights.data[to.update.gen,], FUN = sum)
        As.ind <- match(with(weights.data, genus), with(As, genus))
        As.ind <- As.ind[!is.na(As.ind)]

        weights.data[to.update.gen,"As"] <- As[As.ind, "Ts"]

        # Calculate Tg
        weights.data <- within(weights.data, Tg <- 1*((As/Bs)==1))

        # Calculate Ag:
        # Sum Ts values across genus, family and orders.  
        Ag <- aggregate(Tg ~ genus + family, weights.data[to.update.fam, ], FUN = sum)
        Ag <- aggregate(Tg ~ family, Ag, FUN = function(x) sum(x>0))

        # Merge to weights.data
        Ag.ind <- match(with(weights.data, family), with(Ag, family))
        Ag.ind <- Ag.ind[!is.na(Ag.ind)]
        weights.data[to.update.fam,"Ag"] <- Ag[Ag.ind, "Tg"]

        # Calculate Tf
        weights.data <- within(weights.data, Tf <- 1*((Ag/Bg)==1))

        # Calculate Af:
        # Sum Tf values across family and order
        Af <- aggregate(Tf ~ family + order, weights.data[to.update.ord, ], FUN = sum)
        Af <- aggregate(Tf ~ order, Af, FUN = function(x) sum(x>0))

        # Merge to weights.data
        Af.ind <- match(with(weights.data, order), with(Af, order))
        Af.ind <- Af.ind[!is.na(Af.ind)]
        weights.data[to.update.ord,"Af"] <- Af[Af.ind, "Tf"]

        # Updating complete.  

        # 2.) Find the 'affected list'.  
        # 2.1.) Find those species_ids that were in the same genus as a removed species.  
        same.gen <- weights.data[to.update.gen, "species_id"]
        same.ord <- weights.data[to.update.ord, "species_id"]

        # 2.2.) Find those species_ids that are the last in their genus 
        # (this is takes into account more species than is necessary but it is concise code)
        last.spp <- unlist(subset(weights.data, subset = (Bs - As) == 1, select = species_id))

        # 3.) Intersect the remaining list and the affected list
        # Those species that are in the same order as the removed species AND are the last in their genus.  
        affected <- intersect(same.ord, last.spp)

        # Include those species that are in the same genera as the removed species
        affected <- union(affected, same.gen)

        # Remove species that were in the just.removed vector  
        affected <- intersect(affected, remaining)
        affected <- setdiff(affected, just.removed)

        ans <- list(w = weights.data, aff = affected)
        ans

    }
