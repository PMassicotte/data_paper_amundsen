# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Util functions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

## 2D interpolation

interpolate_to_tibble <- function(df, x, y, z, n = 200) {

  df <- df %>%
    select({{x}}, {{y}}, {{z}}) %>%
    drop_na() %>%
    mba.surf(no.X = n, no.Y = n, sp = TRUE, extend = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(
      {{x}} := xyz.est.x,
      {{y}} := xyz.est.y,
      {{z}} := xyz.est.z
    )

  return(df)

}

# Ocan dataview color -----------------------------------------------------

# I do not like this scale, but I will do what I am told :)
color <- c("#ECC5ED", "#E8AFE7", "#E491E0", "#E078DA", "#DC65D4", "#D84FCF", "#D548CA", "#D151C5", "#CD69C0", "#C87BBD", "#C38FBE", "#BA99C1", "#AF9BC8", "#9D95D2", "#8985DD", "#7270E7", "#5956F2", "#4643FA", "#333DFD", "#263FFA", "#1C45F3", "#1A50EE", "#1B60EA", "#2272E8", "#2C88EA", "#3898EF", "#45AEF5", "#53BFF9", "#60CCFA", "#6ADCF4", "#72E6ED", "#77ECE2", "#78EFD8", "#75EECE", "#6FE9C1", "#66E1B2", "#5BD8A1", "#4ECE91", "#40C682", "#33BF75", "#27BD6A", "#1CBD60", "#13C257", "#0CCA4E", "#07D544", "#05DD3C", "#06E236", "#09E531", "#0FE42D", "#17E02B", "#21DA2B", "#2CD42D", "#38CE31", "#44C937", "#4FC63F", "#5AC547", "#65C550", "#6FC758", "#79CA5F", "#83D064", "#8CD767", "#97DE68", "#A2E567", "#ADEB64", "#BAEF5F", "#C6EF58", "#D3EC4F", "#E0E444", "#EADA3B", "#F2CF33", "#F7C82C", "#F9BF26", "#FAB921", "#FBB31C", "#FCAC18", "#FCA514", "#FCA011", "#FC990E", "#FA930B", "#F88D09", "#F48607", "#EE8105", "#E87A04", "#E27202", "#DD6A02", "#DA6201", "#DA5901", "#DE4D02", "#E54402", "#EE3A03", "#F52C05", "#FA1F07", "#FB1309", "#F9090B", "#F3020E", "#EB0012", "#E10016", "#DA021A", "#D5051F", "#D30924", "#D30F2B", "#D51632", "#D72039", "#DB2D41", "#DF3C4A", "#E44D51", "#EA605A", "#F07462", "#F68A6D", "#FBA078", "#FEB483", "#FFC48E", "#FFC690")
