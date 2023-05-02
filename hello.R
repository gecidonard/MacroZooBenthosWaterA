#'Assigned tolerancevalues range from 0 to 10 for families and increase as water quality decreases
#' @param data Data set must have predefined column names: Family, Abundance, and Site
#' @param data For the families that we  have given the value for particular genera and you should add data like this : Sisyridae_Climacia_sp Talitridae_Hyalellidae, Pelecypoda_Bivalvia, Glossiphoniidae_Helobdella,Glossiphoniidae_other
#'  Chironomidae_red, Chironomidae_other
#' @param tolerances tolerance value are given by default
#' @param print_results A logical value that determines whether or not to print the results. Defaults to TRUE.
#'
#' @return will give values from 0-10
#' @references 1. Hilsenhoff, W.L. 1988. Rapid field assessment of organic pollution with a family- level biotic index. J. N. Am. Benthol. Soc. 7(1):65-68. Barbour, M.T., Gerritsen, J., Snyder,
#' @references 2. Bode, R.W., Novak, M.A., and Abele, L.E. 1996. Quality Assurance Work Plan for Biological Stream Monitoring in New York State. NYS Department of Environmental Conservation, Albany, NY. 89p. 4 appendices.
#' @references 3. Bode, R.W., Novak, M.A., Abele, L.E., Heitzman, D.L., and Smith, A.J. 2002. Quality Assurance Work Plan for Biological Stream Monitoring in New York State. NYS Department of Environmental Conservation, Albany, NY. 115p
#' @references 4. Hauer, F.R., Lamberti, G.A. (eds.) 1996. Methods in Stream Ecology. Academic Press. ISBN: 0-12-332906-X. 696pp.
#' @references 5.B.D., Stribling, J.B. 1999.Rapid BioassessmentProtocols For Use in Streams and Wadeable Rivers:Periphyton, BenthicMacroinvertebrates, and Fish. Second Edition. EPA 841-B-99-002. Washington, D.C. xiv, 11chapters,
#' @references 6. Plafkin, J.L. , Barbour, M.T., Porter, K.D., Gross, S.K., and Hughes, R.M.. 1989. Rapid Bioassessment Protocols for use in Streams and Rivers: Benthic Macroinvertebrates and Fish. U.S. Environmental Protection Agency. EPA 440/4-  89/001. 8 chapters, Appendices A-D.
#'@examples
#' Site= c("L1", "L2", "L2")
#' Family=c("Capniidae",  "Aeshnidae", "Chloroperlidae")
#' Abundance=c(4,2,15)
#' test <- data.frame( Family, Abundance, Site)
#' calculate_FBI(test)
#'@export
calculate_FBI <- function(data, tolerances = list(
  Capniidae= 1,
  Aeshnidae= 3,
  Chloroperlidae= 1,
  Calopterygidae= 5,
  Leuctridae= 0,
  Coenagrionidae= 9,
  Nemouridae= 2,
  Cordulegastridae= 3,
  Perlidae= 1,
  Corduliidae= 5,
  Perlodidae= 2,
  Gomphidae= 1,
  Pteronarcyidae= 0,
  Lestidae= 9,
  Taeniopterygidae= 2,
  Libellulidae= 9,
  Macromiidae= 3,
  Baetidae= 4,
  Baetiscidae= 3,
  Athericidae= 2,
  Caenidae= 7,
  Blephariceridae= 0,
  Ephemerellidae= 1,
  Ceratopogonidae= 6,
  Ephemeridae= 4,
  Chironomidae_red= 8,
  Heptageniidae= 4,
  Chironomidae_other= 6,
  Leptophlebiidae= 2,
  Dolichopodidae= 4,
  Metrtopodidae= 2,
  Empididae= 6,
  Oligoneuriidae= 2,
  Ephydridae= 6,
  Polymitarcyidae= 2,
  Psychodidae= 10,
  Potamanthidae= 4,
  Simuliidae =6,
  Siphlonuridae= 7,
  Muscidae= 6,
  Tricorythidae= 4,
  Syrphidae= 10,
  Tabanidae= 6,
  Pyralidae= 5,
  Tipulidae= 3,
  Brachycentridae= 1,
  Dryopidae=5,
  Calamoceratidae= 3,
  Elmidae= 4,
  Glossosomatidae= 1,
  Psephenidae= 4,
  Helicopsychidae= 3,
  Hydropsychidae= 4,
  Gammaridae= 4,
  Hidroptilidae= 4,
  Hyalellidae= 8,
  Lepidostomatidae= 1,
  Leptoceridae= 4,
  Asellidae= 8,
  Limnephilidae= 4,
  Molannidae= 6,
  Odontoceridae= 0,
  Astacidae= 6,
  Philopotamidae= 3,
  Phryganeidae= 4,
  Lymnaeidae= 6,
  Polycentropodidae= 6,
  Physidae= 8,
  Psychomyiidae= 2,
  Rhyacophilidae= 0,
  Pisidiidae= 8,
  Sericostomatidae= 3,
  Oligochaeta= 8,
  Uenoidae= 3,
  Hirudinea= 10,
  Turbellaria= 4,
  Corydalidae= 0,
  Sialidae= 4,
  Ameletidae= 0,
  Baetiscidae= 4,
  Isonychiidae= 2,
  Leptohyphidae= 4,
  Metretopodidae= 2,
  Potomanthidae= 4,
  Peltoperlidae= 0,
  Corixidae= 5,
  Apataniidae= 3,
  Dipseudopsidae= 5,
  Goeridae= 3,
  Curculionidae= 5,
  Dytiscidae= 5,
  Gyrinidae= 4,
  Haliplidae= 5,
  Hydrophilidae= 5,
  Ptilodactylidae= 3,
  Scirtidae= 5,
  Sisyridae_Climacia_sp = 5,
  Anthomyiidae= 6,
  Chaoboridae= 8,
  Culicidae= 8,
  Dixidae= 1,
  Ptychopteridae= 1,
  Scathophagidae= 6,
  Stratiomyidae= 7,
  Tanyderidae= 3,
  Arrenuridae= 6,
  Lebertiidae= 6,
  Atractideidae= 6,
  Mideopsidae= 6,
  Tyrellidae= 6,
  Limnesidae= 6,
  Limnocharidae= 6,
  Sperchonidae= 6,
  Unionicolidae= 6,
  Polydesmida= 6,
  Anthuridae= 5,
  Idoteidae= 5,
  Crangonyctidae= 6,
  Oedicerotidae= 5,
  Talitridae_Hyalellidae= 8,
  Acariformes= 4,
  Ostracoda= 8,
  Planorbidae= 7,
  Ancylidae= 6,
  Viviparidae= 6,
  Pleuroceridae= 6,
  Bithyniidae= 8,
  Hydrobiidae= 6,
  Valvatidae= 8,
  Pelecypoda_Bivalvia= 8,
  Unionidae= 6,
  Corbiculidae= 6,
  Dreisseniidae= 8,
  Sphaeriidae= 6,
  Pisidiidae= 8,
  Haplotaxidae= 5,
  Lumbricida= 6,
  Lumbriculidae= 5,
  Enchytraeidae= 10,
  Tubificidae= 9,
  Naididae= 8,
  Glossiphoniidae_Helobdella= 6,
  Glossiphoniidae_other= 8,
  Aeolosomatidae= 8,
  Branchiobdellidae= 6,
  Sabellidae= 6,
  Platyhelminthidae= 4

), print_results = TRUE) {

  calculate_site_FBi <- function(site_data) {
    sums <- sapply(names(tolerances), function(family) {

      if (sum(site_data$Family == family) == 0) {
        return(0)
      }
      sum(site_data[site_data$Family == family, "Abundance"] * tolerances[[family]])
    })
    index <- sum(sums) / sum(site_data$Abundance)
    if (index >= 0.00 & index <= 3.75) {
      pollution_degree <- "Excellent"
      pollution_likelihood <- "Organic pollution unlikely"
    } else if (index > 3.75 & index <= 4.25) {
      pollution_degree <- "Very Good"
      pollution_likelihood <- "Possible slight organic pollution"
    } else if (index > 4.25 & index <= 5.00) {
      pollution_degree <- "Good"
      pollution_likelihood <- "Some organic pollution probable"
    } else if (index > 5.00 & index <= 5.75) {
      pollution_degree <- "Fair"
      pollution_likelihood <- "Fairly substantial pollution likely"
    } else if (index > 5.75 & index <= 6.50) {
      pollution_degree <- "Fairly poor"
      pollution_likelihood <- "Substantial pollution likely"
    } else if (index > 6.50 & index <= 7.25) {
      pollution_degree <- "Poor"
      pollution_likelihood <- "Very substantial pollution likely"
    } else {
      pollution_degree <- "Very poor"
      pollution_likelihood <- "Severe organic pollution likely"
    }

    return(list(FBI = index,
                Water_Quality = pollution_degree,
                Degree_of_Organic_Pollution = pollution_likelihood))
  }


  results <- lapply(split(data, data$Site), calculate_site_FBi)

  if (print_results) {
    print(results)
  }


}


#' This index is calculated based on the number of families belonging to three macroinvertebrate orders: Ephemeroptera, Plecoptera and Trichoptera.
#' @param dataset Data set must have predefined column names: Family, Abundace and Site
#' @param EPT_families families that belong  to EPT are given by default
#' @param print_results A logical value that determines whether or not to print the results. Defaults to TRUE.
#'
#' @return values from 0-10,
#' @references Hilsenhoff, W.L. 1987. An Improved Biotic Index of Organic Stream Pollution. Michigan Entomology Society. 20(11):9-13
#' @references Lenat, D.R. 1988. Water quality assessment using a qualitative collection method for benthic macroinvertebrates. J.N. Am. Benthological Soc. 7: 222-233.
#' @references MacDonald, D.D., Ingersoll, C.G., and Berger, T.A., 2000, Development and evaluation of consensus-based sediment quality guidelines for freshwater ecosystems: Archives of Environmental Contamination and Toxicology, v. 39, p. 20-31.
#' @references Weber, C.I., ed., 1973, Biological field and laboratory methods for measuring the quality of surface waters and effluents: Cincinnati, Ohio, U.S. Environmental Protection Agency, EPA-670/4-73-001.
#' @references  Lenat DR, Penrose DL. History of the EPT taxa richness metric. Bulletin North American Benthological Society. 1996;12:279–290. https://doi.org/10.2307/1467463.
#'@examples
#' Site= c("L1", "L2", "L2")
#' Family=c("Capniidae",  "Aeshnidae", "Chloroperlidae")
#' test <- data.frame( Family, Site)
#' calculate_EPT(test)
#'@export
calculate_EPT <- function(dataset, EPT_families  = c("Ameletidae",
                                                     "Ametropodidae",
                                                     "Ameletopsidae",
                                                     "Arthropleidae",
                                                     "Acanthametropodidae",
                                                     "Baetidae",
                                                     "Baetiscidae",
                                                     "Behningiidae",
                                                     "Caenidae",
                                                     "Coloburiscidae",
                                                     "Ephemerellidae",
                                                     "Ephemeridae",
                                                     "Heptageniidae",
                                                     "Isonychiidae",
                                                     "Leptophlebiidae",
                                                     "Leptohyphidae",
                                                     "Metretopodidae",
                                                     "Neoephemeridae",
                                                     "Oligoneuriidae",
                                                     "Palingeniidae",
                                                     "Prosopistomatidae",
                                                     "Polymitarcyidae",
                                                     "Potamanthidae",
                                                     "Siphlonuridae",
                                                     "Tricorythidae",
                                                     "Oniscigastridae",
                                                     "Austroperlidae",
                                                     "Capniidae",
                                                     "Chloroperlidae",
                                                     "Diamphipnoidae",
                                                     "Eustheniidae",
                                                     "Gripopterygidae",
                                                     "Leuctridae",
                                                     "Nemouridae",
                                                     "Notonemouridae",
                                                     "Peltoperlidae",
                                                     "Perlidae",
                                                     "Perlodidae",
                                                     "Pteronarcyidae",
                                                     "Taeniopterygidae",
                                                     "Styloperlidae",
                                                     "Anomalopsychidae",
                                                     "Antipodoeciidae",
                                                     "Apataniidae",
                                                     "Arctopsychidae",
                                                     "Atriplectididae",
                                                     "Beraeidae",
                                                     "Brachycentridae",
                                                     "Calamoceratidae",
                                                     "Chathamiidae",
                                                     "Dipseudopsidae",
                                                     "Ecnomidae",
                                                     "Glossosomatidae",
                                                     "Goeridae",
                                                     "Helicopsychidae",
                                                     "Hydrobiosidae",
                                                     "Hydropsychidae",
                                                     "Hydroptilidae",
                                                     "Kalophryganeidae",
                                                     "Kokiriidae",
                                                     "Lepidostomatidae",
                                                     "Leptoceridae",
                                                     "Limnephilidae",
                                                     "Molannidae",
                                                     "Odontoceridae",
                                                     "Oeconesidae",
                                                     "Philopotamidae",
                                                     "Philorheithridae",
                                                     "Phryganeidae",
                                                     "Polycentropodidae",
                                                     "Psychomyiidae",
                                                     "Pisuliidae",
                                                     "Plectrotarsidae",
                                                     "Rhyacophilidae",
                                                     "Stenopsychidae",
                                                     "Sericostomatidae",
                                                     "Tasimiidae",
                                                     "Uenoidae",
                                                     "Xiphocentronidae"),
                          print_results = TRUE){

  dataset_list <- split(dataset, dataset$Site)


  results <- lapply(dataset_list, function(df) {

    df <- df[df$Abundance != 0,]


    EPT_families_in_dataset <- unique(df$Family[df$Family %in% EPT_families])


    total_EPT_families <- length(EPT_families_in_dataset)


    if (total_EPT_families < 2) {
      EPT_Index <- "Polluted"
    } else if (total_EPT_families >= 2 & total_EPT_families <= 5) {
      EPT_Index <- "Clean"
    } else if (total_EPT_families >= 6 & total_EPT_families <= 10) {
      EPT_Index <- "Good"
    } else {
      EPT_Index <- "Very Good"
    }



    results= list(EPT_Index = EPT_Index, Total_EPT_Families = total_EPT_families)
  })

  if (print_results) {
    print(results)
  }


}

#' The Biological Monitoring Working Party score (BMWP) This index provides single values, at the family level (with exception of Oligochaete which is to  the order level), representative of the organisms’ tolerance to pollution.

#' @param dataset Data set must have predefined column names: Family and Site
#'
#' @param tolerances tolerance value are given by default
#' @param Site Site should given as default to your data
#' @param print_results A logical value that determines whether or not to print the results. Defaults to TRUE.
#' @return will give values from 0-100 of BMWP index, Water Quality and Level of Pollution for each Site
#' @references Paisley M.F., Trigg D.J. & Walley W.J. 2013. Revision of the Biological Monitoring Working Party (BMWP) Score System: Derivation of present-only and abundance-related scores from field data. River Res. Appl. 30 (7): 887–904. DOI: 10.1002/rra.2686
#' @references Zeybek M., Kalyoncu H., Karakaş B. & Özgül S. 2014. The use of BMWP and ASPT indices for evaluation of water quality according to macroinvertebrates in Değirmendere Stream (Isparta, Turkey). Turk. J. Zool. 38: 603–613. DOI: 10.3906/zoo-1310-9
#' @references Arslan, N., Salur, A., Kalyoncu, H. et al. The use of BMWP and ASPT indices for evaluation of water quality according to macroinvertebrates in Küçük Menderes River (Turkey). Biologia 71, 49–57 (2016). https://doi.org/10.1515/biolog-2016-0005
#' @references Biological Monitoring Working Party Final Report: Assessment and Presentation of Biological Quality of Rivers in Great Britain. December 1978, Department of the Environment, Water Data Unit (1978), p. 37
#' @references L. Li, B. Zheng, L. Liu Biomonitoring and bioindicators used for river ecosystems: definitions, approaches and trends Procedia Environ. Sci., 2 (2010), pp. 1510-1524, 10.1016/j.proenv.2010.10.164
#' @references National Water Council River Quality: the 1981 Survey and Future Outlook National Water Council, London, UK (1981), p. 39
#' @references J. Alba-Tercedor Macroinvertebrados acuáticos y calidad de las aguas de los ríos. IV Simposio del agua en Andalucía (SIAGA) Almería, 2 (1996), pp. 203-231
#' @references S. Guareschi, A. Laini, M.M. Sánchez-Montoya How do low-abundance taxa affect river biomonitoring? Exploring the response of different macroinvertebrate-based indices J. Limnol., 76 (s1) (2017), pp. 9-20, 10.4081/jlimnol.2016.1516
#' @references S.E. Mustow Biological monitoring of rivers in Thailand: use and adaptation of the BMWP Score Hydrobiologia, 479 (2002), pp. 199-229
#'@examples
#' Site= c("L1", "L2", "L2")
#' Family=c("Capniidae",  "Aeshnidae", "Chloroperlidae")
#' test <- data.frame( Family, Site)
#' calculate_BMWP(test)
#'
#' @export
calculate_BMWP <- function(dataset, Site = "Site", tolerances = list(Siphlonuridae = 10, Heptageniidae = 10, Leptophlebiidae = 10, Ephemerellidae = 10,
                                                                    Potamanthidae = 10, Ephemeridae = 10, Taeniopterygidae = 10, Leuctridae = 10,
                                                                    Capniidae = 10, Perlodidae = 10, Perlidae = 10, Chloroperlidae = 10,
                                                                    Aphelocheridae = 10, Phryganeidae = 10, Molannidae = 10, Beraeidae = 10,
                                                                    Odontoceridae = 10, Leptoceridae = 10, Goeridae = 10, Lepidostomatidae = 10,
                                                                    Brachycentridae = 10, Sericostomatidae = 10, Astacidae = 8, Lestidae = 8,
                                                                    Agriidae = 8, Gomphidae = 8, Cordulegasteridae = 8, Aeshnidae = 8,
                                                                    Corduliidae = 8, Libellulidae = 8, Caenidae = 7, Nemouridae = 7,
                                                                    Rhyacophilidae = 7, Polycentropodidae = 7, Limnephilidae = 7,
                                                                    Neritidae = 6, Viviparidae = 6, Ancylidae = 6, Hydroptilidae = 6,
                                                                    Unionidae = 6, Platycnemididae = 6, Coenagriidae = 6,
                                                                    Mesoveliidae = 5, Hydrometridae = 5, Gerridae = 5, Nepidae = 5, Naucoridae = 5,
                                                                    Notonectidae = 5, Pleidae = 5, Corixidae = 5, Haliplidae = 5, Hygrobiidae = 5,
                                                                    Dytiscidae = 5, Gyrinidae = 5, Hydrophilidae = 5, Clambidae = 5, Helodidae = 5,
                                                                    Dryopidae = 5, Elmidae = 5, Chrysomelidae = 5, Curculionidae = 5, Hydropsychidae = 5,
                                                                    Tipulidae = 5, Simuliidae = 5,  Planariidae = 5, Dendrocoelidae = 5, Baetidae = 4,
                                                                    Sialidae = 4, Piscicolidae = 4, Valvatidae = 3, Hydrobiidae = 3, Lymnaeidae = 3, Physidae= 3,
                                                                    Planorbidae = 3, Sphaeriidae = 3, Glossiphoniidae = 3, Hirudidae = 3, Erpobdellidae = 3,
                                                                    Asellidae = 3, Chironomidae = 2, Oligochaeta = 1, Cypermethrin = 0 ),

print_results = TRUE) {

  tol_dict <- as.list(tolerances)


  results <- data.frame(Site = character(),
                        BMWP = numeric(),
                        Quality = character(),
                        stringsAsFactors = FALSE)


  for (site in unique(dataset$Site)) {

    site_data <- subset(dataset, Site == site)


    tol_sum <- sum(unlist(tol_dict[match(site_data$Family, names(tol_dict))]))


    num_families <- length(tolerances)
    BMWP <- tol_sum


    if (BMWP >= 0 & BMWP <= 10) {
      category <- "Very poor"
      interpretation <- "Heavily polluted"
    } else if (BMWP >= 11 & BMWP <= 40) {
      category <- "Poor"
      interpretation <- "Polluted or impacted"
    } else if (BMWP >= 41 & BMWP <= 70) {
      category <- "Moderate"
      interpretation <- "Moderately impacted"
    } else if (BMWP >= 71 & BMWP <= 100) {
      category <- "Good"
      interpretation <- "Clean but slightly impacted"
    } else {
      category <- "Very good"
      interpretation <- "Unpolluted, unimpacted"
    }


    results <- rbind(results, data.frame(Site = site, BMWP = BMWP, Quality = category, Pollution=interpretation))
  }

  if (print_results) {
    print(results)
  }




}



#' The Average Score Per Taxon (ASPT) represents the average tolerance score of all taxa within the community, and is calculated by dividing the BMWP by the number of families/taxa represented in the sample.
#' @param dataset data must have Family and Site
#' @param tolerances this parameter is given by default
#' @param Site Site should given as default to your data
#' @param print_results A logical value that determines whether or not to print the results. Defaults to TRUE.
#' @return will give values  of ASPT index from 0-10 and Pollution degree for each site

#' @references Paisley M.F., Trigg D.J. & Walley W.J. 2013. Revision of the Biological Monitoring Working Party (BMWP) Score System: Derivation of present-only and abundance-related scores from field data. River Res. Appl. 30 (7): 887–904. DOI: 10.1002/rra.2686
#' @references Zeybek M., Kalyoncu H., Karakaş B. & Özgül S. 2014. The use of BMWP and ASPT indices for evaluation of water quality according to macroinvertebrates in Değirmendere Stream (Isparta, Turkey). Turk. J. Zool. 38: 603–613. DOI: 10.3906/zoo-1310-9
#' @references Arslan, N., Salur, A., Kalyoncu, H. et al. The use of BMWP and ASPT indices for evaluation of water quality according to macroinvertebrates in Küçük Menderes River (Turkey). Biologia 71, 49–57 (2016). https://doi.org/10.1515/biolog-2016-0005
#' @references Biological Monitoring Working Party Final Report: Assessment and Presentation of Biological Quality of Rivers in Great Britain. December 1978, Department of the Environment, Water Data Unit (1978), p. 37
#' @references L. Li, B. Zheng, L. Liu Biomonitoring and bioindicators used for river ecosystems: definitions, approaches and trends Procedia Environ. Sci., 2 (2010), pp. 1510-1524, 10.1016/j.proenv.2010.10.164
#' @references National Water Council River Quality: the 1981 Survey and Future Outlook National Water Council, London, UK (1981), p. 39
#' @references J. Alba-Tercedor Macroinvertebrados acuáticos y calidad de las aguas de los ríos. IV Simposio del agua en Andalucía (SIAGA) Almería, 2 (1996), pp. 203-231
#' @references S. Guareschi, A. Laini, M.M. Sánchez-Montoya How do low-abundance taxa affect river biomonitoring? Exploring the response of different macroinvertebrate-based indices J. Limnol., 76 (s1) (2017), pp. 9-20, 10.4081/jlimnol.2016.1516
#' @references S.E. Mustow Biological monitoring of rivers in Thailand: use and adaptation of the BMWP Score Hydrobiologia, 479 (2002), pp. 199-229
#'@examples
#' Site <- c("L1", "L2", "L2")
#' Family <- c("Capniidae", "Aeshnidae", "Chloroperlidae")
#' test <- data.frame(Family, Site)
#' calculate_ASPT(test)
#' @export
calculate_ASPT<- function(dataset,  Site = "Site", tolerances = list(Siphlonuridae = 10, Heptageniidae = 10, Leptophlebiidae = 10, Ephemerellidae = 10,
                                                                     Potamanthidae = 10, Ephemeridae = 10, Taeniopterygidae = 10, Leuctridae = 10,
                                                                     Capniidae = 10, Perlodidae = 10, Perlidae = 10, Chloroperlidae = 10,
                                                                     Aphelocheridae = 10, Phryganeidae = 10, Molannidae = 10, Beraeidae = 10,
                                                                     Odontoceridae = 10, Leptoceridae = 10, Goeridae = 10, Lepidostomatidae = 10,
                                                                     Brachycentridae = 10, Sericostomatidae = 10, Astacidae = 8, Lestidae = 8,
                                                                     Agriidae = 8, Gomphidae = 8, Cordulegasteridae = 8, Aeshnidae = 8,
                                                                     Corduliidae = 8, Libellulidae = 8, Caenidae = 7, Nemouridae = 7,
                                                                     Rhyacophilidae = 7, Polycentropodidae = 7, Limnephilidae = 7,
                                                                     Neritidae = 6, Viviparidae = 6, Ancylidae = 6, Hydroptilidae = 6,
                                                                     Unionidae = 6, Platycnemididae = 6, Coenagriidae = 6,
                                                                     Mesoveliidae = 5, Hydrometridae = 5, Gerridae = 5, Nepidae = 5, Naucoridae = 5,
                                                                     Notonectidae = 5, Pleidae = 5, Corixidae = 5, Haliplidae = 5, Hygrobiidae = 5,
                                                                     Dytiscidae = 5, Gyrinidae = 5, Hydrophilidae = 5, Clambidae = 5, Helodidae = 5,
                                                                     Dryopidae = 5, Elmidae = 5, Chrysomelidae = 5, Curculionidae = 5, Hydropsychidae = 5,
                                                                     Tipulidae = 5, Simuliidae = 5,  Planariidae = 5, Dendrocoelidae = 5, Baetidae = 4,
                                                                     Sialidae = 4, Piscicolidae = 4, Valvatidae = 3, Hydrobiidae = 3, Lymnaeidae = 3, Physidae= 3,
                                                                     Planorbidae = 3, Sphaeriidae = 3, Glossiphoniidae = 3, Hirudidae = 3, Erpobdellidae = 3,
                                                                     Asellidae = 3, Chironomidae = 2, Oligochaeta = 1, Cypermethrin = 0 ),
print_results = TRUE) {


  tol_dict <- as.list(tolerances)


  results <- data.frame(Site = character(),
                        ASPT = numeric(),
                        Quality = character(),
                        stringsAsFactors = FALSE)


  for (site in unique(dataset$Site)) {

    site_data <- subset(dataset, Site == site)


    tol_sum <- sum(unlist(tol_dict[match(site_data$Family, names(tol_dict))]))


    num_families <- length(tolerances)
    ASPT <- tol_sum / num_families


    if (ASPT > 6) {
      quality <- "Clean Water"
    } else if (ASPT > 5) {
      quality <- "Doubtful quality"
    } else if (ASPT > 4) {
      quality <- "Probable moderate pollution"
    } else {
      quality <- "Probable severe pollution"
    }


    results <- rbind(results, data.frame(Site = site, ASPT = ASPT, Quality = quality))
  }

  if (print_results) {
    print(results)
  }


}





#' SWRC - Biotic index (Stroud Water Research Centre - Biotic index)
#This index is similar with FBI but most of the tolerace values are given to the order level only some families are appointed  with tolerance value

#' @param data A data frame with predefined column names: Family, Abundance and Site. The dataset should be at the order level. If the family is not at the order level, it should be linked with an underscore (e.g., Trichoptera_Hydropsychidae).
#' @param tolerances Tolerance values are given by default.
#' @param print_results A logical value that determines whether or not to print the results. Defaults to TRUE.
#' @return A data frame with two columns: SWRC Index (0-10) and Water Quality for each site.
#' @references Schmiedt, K., Jones, R. L., Brill, I. & Pikal, W. (1998). EPT (Epheromeraptera, Plecoptera and Trichoptera) Family Richness Modified Biotic Index.
#' SWRC – Stroud Water Research Centre (2023). Leaf Pack Network: Watersheds. Available from: www.stroudcenter.org/lpn/more/data.
#' SWRC (STROUD WATER RESEARCH CENTER) (2003). Water quality monitoring in the source water areas for New York City: an integrative approach. A report on the first phase of monitoring. Stroud Water Research Center, Avondale, PA, U.S.A.
#' @examples
#' Site= c("L1", "L2", "L2")
#' Family=c("Capniidae",  "Aeshnidae", "Chloroperlidae")
#' Abundance=c(4,2,15)
#' test <- data.frame( Family, Abundance, Site)
#' calculate_SWRC(test)
#' @export

calculate_SWRC<- function(data, tolerances = list(
  Trichoptera_Hydropsychidae= 2.8,
  Trichoptera= 5,
  Plecoptera= 1,
  Ephemeroptera= 3.6,
  Diptera_Athericidae= 2,
  Diptera_Chironomidae_white= 6,
  Diptera_Chironomidae_red= 8,
  Diptera_Tipulidae= 3,
  Diptera_other= 6,
  Amphipoda= 6,
  Gastropoda= 7,
  Oligochaeta= 8,
  Hirudinea= 8,
  Coleoptera= 4.6,
  Mollusca= 7,
  Nematoda= 8,
  Coleoptera= 4.6,
  Megaloptera= 3,
  Odonata_Zygoptera=7,
  Isopda= 8,
  Decapoda= 5,
  Hemiptera=0

), print_results = TRUE) {

  results <- lapply(split(data, data$Site), function(site_data) {
    sums <- sapply(names(tolerances), function(family) {
      abundance <- site_data[site_data$Family == family, "Abundance"]
      if (sum(abundance) > 0) {
        sum(abundance * tolerances[[family]])
      } else {
        0
      }
    })


    total_abundance <- sum(site_data$Abundance)
    if (total_abundance == 0) {
      index <- 0
    } else {
      index <- sum(sums) / total_abundance
    }


    if (index >= 0.00 & index <= 3.75) {
      pollution_degree <- "Excellent"
    } else if (index > 3.76 & index <= 5.0) {
      pollution_degree <- "Good"
    } else if (index > 5.10 & index <= 6.50) {
      pollution_degree <- "Fair"
    } else if (index > 6.60 & index <=10.00) {
      pollution_degree <- "Poor"
    } else {
      pollution_degree <- "Unknown"
    }


  })

  if (print_results) {
    print(results)
  }


}

