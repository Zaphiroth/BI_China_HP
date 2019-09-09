# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  BI_CORP
# Purpose:      Data Cleaning
# programmer:   Zhe Liu
# Date:         09-09-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##---- load raw data ----
raw.header <- read.xlsx("./Raw_data/Corporation.xlsx",
                        na.strings = "NA")

raw.china <- read.csv("./Raw_data/CHINA CORPORATION.CSV",
                      na.strings = "NA",
                      stringsAsFactors = FALSE,
                      fileEncoding = "GB2312")

raw.region <- read.csv("./Raw_data/REGION CORPORATION.CSV",
                       na.strings = "NA",
                       stringsAsFactors = FALSE,
                       fileEncoding = "GB2312")

raw.province <- read.csv("./Raw_data/PROVINCE CORPORATION.CSV",
                         na.strings = "NA",
                         stringsAsFactors = FALSE,
                         fileEncoding = "GB2312")

##---- calculate new columns function ----
CalculateColumns <- function(content, measure, col.header) {
  
  for (i in sort(grep(paste0("qtr_", measure), col.header, value = TRUE, fixed = TRUE))) {
    position <- which(colnames(content) == paste0("mth", substr(i, 4, nchar(i))))
    if (length(position) == 1 && position >= 3) {
      content[i] <- content[position] + content[position - 1] + content[position - 2]
    }
  }
  
  for (i in sort(grep(paste0("ytd_", measure), col.header, value = TRUE, fixed = TRUE))) {
    position <- which(colnames(content) == paste0("mth", substr(i, 4, nchar(i))))
    if (length(position) == 1 && position >= 1) {
      content[i] <- content[position]
      for (j in 1:11) {
        if (substr(colnames(content)[position - j], 14, 17) == substr(colnames(content)[position], 14, 17)) {
          content[i] <- content[i] + content[position - j]
        } else {
          break()
        }
      }
    }
  }
  
  for (i in sort(grep(paste0("mat_", measure), col.header, value = TRUE, fixed = TRUE))) {
    position <- which(colnames(content) == paste0("mth", substr(i, 4, nchar(i))))
    if (length(position) == 1 && position >= 12) {
      content[i] <- content[position]
      for (j in 1:11) {
        content[i] <- content[i] + content[position - j]
      }
    }
  }
  
  for (i in sort(grep(paste0("yrl_", measure), col.header, value = TRUE, fixed = TRUE))) {
    position <- which(colnames(content) == paste0("mth", substr(i, 4, nchar(i)-2), "12"))
    if (length(position) == 1 && position >= 12) {
      content[i] <- content[position]
      for (j in 1:11) {
        content[i] <- content[i] + content[position - j]
      }
    }
  }
  
  return(content)
}

##---- cleaning function ----
ChinaHPCleaning <- function(raw.data, raw.header) {
  
  ##---- rename raw data ----
  header <- data.frame("raw.col" = tolower(colnames(raw.data))) %>% 
    filter(substr(raw.col, 1, 3) == "mth") %>% 
    mutate(raw.col = gsub(pattern = "[.]", replacement = "", x = raw.col),
           period = substr(raw.col, 1, 3),
           month = substr(raw.col, 4, 5),
           year = substr(raw.col, 6, 9),
           measure = substr(raw.col, 10, 10)) %>% 
    mutate(measure = ifelse(measure == "",
                            "RENMINBI",
                            "UNIT")) %>% 
    select("period", "measure", "year", "month") %>% 
    unite(col = "date", "year", "month", sep = ".") %>% 
    unite(col = "col", "period", "measure", "date", sep = "_")
  
  content <- raw.data[, which(substr(colnames(raw.data), 1, 3) == "MTH")] %>% 
    filter(row_number() != 1)
  content[] <- lapply(content, as.numeric)
  colnames(content) <- header$col
  
  rmb <- content[, grep("RENMINBI", colnames(content), value = FALSE, fixed = TRUE, invert = FALSE)]
  unit <- content[, grep("UNIT", colnames(content), value = FALSE, fixed = TRUE, invert = FALSE)]
  
  ##---- added header ----
  header.add <- grep("mth", colnames(raw.header)[5:length(raw.header)], value = TRUE, fixed = TRUE, invert = TRUE)
  
  ##---- row header ----
  row.header <- raw.data %>% 
    filter(row_number() != 1) %>% 
    select("AUDIT.DESC", "CORPORATE.DESC", "MARKET.DESC", "MANUF.TYPE.DESC") %>% 
    mutate(`MNC/Local` = ifelse(`MANUF.TYPE.DESC` == "IMPORT" | `MANUF.TYPE.DESC` == "JOINT-VENTURE",
                                "MNC",
                                ifelse(`MANUF.TYPE.DESC` == "LOCAL",
                                       "Local",
                                       NA))) %>% 
    select(-`MANUF.TYPE.DESC`)
  
  ##---- add columns ----
  rmb.new <- CalculateColumns(rmb, "RENMINBI", header.add)
  unit.new <- CalculateColumns(unit, "UNIT", header.add)
  
  cleaned <- bind_cols(row.header, rmb.new, unit.new)
  
  return(cleaned)
}

##---- data cleaning ----
china.cleaned <- ChinaHPCleaning(raw.china, raw.header)
region.cleaned <- ChinaHPCleaning(raw.region, raw.header)
province.cleaned <- ChinaHPCleaning(raw.province, raw.header)

write.csv(china.cleaned, "./Format_data/CHINA_CORPORATION_format.csv", row.names = FALSE)
write.csv(region.cleaned, "./Format_data/REGION_CORPORATION_format.csv", row.names = FALSE)
write.csv(province.cleaned, "./Format_data/PROVINCE_CORPORATION_format.csv", row.names = FALSE)



