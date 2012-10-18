CalculateWeightsII <-
function(weights.data, affected, weighting = 1, multiplier = 1, ...){

    if(length(affected) == 0)
        return(weights.data)

    for(i in affected){

        # Subset the dataset for the genus of the species in question
        affected.gen <- weights.data[which(weights.data$species_id == i), "genus"]
        w.sub.spp <- subset(weights.data, subset = genus == affected.gen)

        index.spp <- with(w.sub.spp, which(species_id == i))
        w.sub.spp <- within(w.sub.spp, Ts[index.spp] <- 1)

        # Calculate As as the sum of Ts in the genus.
        w.sub.spp <- within(w.sub.spp, As[index.spp] <- aggregate(Ts ~ genus, w.sub.spp, FUN = sum)$Ts)

        # Set Tg to 1 if all the species in the genus are threatened.
        if(with(w.sub.spp[index.spp, ], As/Bs == 1)) w.sub.spp[index.spp, "Tg"] <- 1
        w.sub <- w.sub.spp

        # If Tg is 1, continue
        if(w.sub.spp[index.spp, "Tg"] == 1){

            # Subset the data again to include all species in the family
            affected.fam <- weights.data[which(weights.data$species_id == i), "family"]
            w.sub.gen <- subset(weights.data, subset = family == affected.fam)

            # Carry over the Ts and As values from the first step...
            index.gen <- with(w.sub.gen, which(species_id == i))
            w.sub.gen <- within(w.sub.gen, {
                    Ts[index.gen] <- with(w.sub.spp, Ts[index.spp])
                    As[index.gen] <- with(w.sub.spp, As[index.spp])
                    Tg[index.gen] <- with(w.sub.spp, Tg[index.spp])
                    })

            # Calculate Ag as the sum of Tg values
            w.sub.gen <- within(w.sub.gen, Ag[index.gen] <- aggregate(Tg ~ family, w.sub.gen, FUN = sum)$Tg)

            # Set Tf to 1 if all genera in the family are threatened

            if(with(w.sub.gen[index.gen, ], Ag/Bg == 1)) w.sub.gen[index.gen,"Tf"] <- 1

            w.sub <- w.sub.gen

            # If Tf is 1, continue
            if(w.sub.gen[index.gen, "Tf"] == 1){

                # Subset the data again to include all species in the order
                affected.ord <- weights.data[which(weights.data$species_id == i), "order"]
                w.sub.fam <- subset(weights.data, subset = order == affected.ord)

                # Carry over the Ts, As, Tg, and Ag values from the previous steps...
                index.fam <- with(w.sub.fam, which(species_id == i))
                w.sub.fam <- within(w.sub.fam, {
                    Ts[index.fam] <- with(w.sub.gen, Ts[index.gen])
                    As[index.fam] <- with(w.sub.gen, As[index.gen])
                    Tg[index.fam] <- with(w.sub.gen, Tg[index.gen])
                    Ag[index.fam] <- with(w.sub.gen, Ag[index.gen])
                    Tf[index.fam] <- with(w.sub.gen, Tf[index.gen])
                    })

                # Calculate Af as the sum of the Tf values
                w.sub.fam <- within(w.sub.fam, Af[index.fam] <- aggregate(Tf ~ order, w.sub.fam, FUN = sum)$Tf)
                w.sub <- w.sub.fam
            }
        }
        # Calculate and save R, D
        R <- 1 + with(w.sub, (As/Bs)*Ts + (Ag/Bg)*Tg + (Af/Bf)*Tf)
        D <- 1 + with(w.sub, (As/Bs==1)*Endem.spp + (Ag/Bg==1)*Endem.gen + (Af/Bf==1)*Endem.fam)

        W <- multiplier*(R*D)^weighting

        j <- with(w.sub, which(species_id == i))
        k <- with(weights.data, which(species_id == i))

        # Assign weight for species in question
        weights.data[k, c("R", "D", "weight_value")] <- c(R[j], D[j], W[j])
    }
    weights.data
}# fn end
