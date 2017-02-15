library(dplyr)
library(tidyr)

refine <- read.csv("refine_original.csv")

##Clean company names by making them lowercase and replacing spelling based on first letter
refine <- refine %>%
  mutate(company = tolower(company)) %>%
  mutate(company = gsub(pattern = "^u.+", replacement = "unilever",
    gsub(pattern = "^[pf].+", replacement = "philips",
    gsub(pattern = "^a.+", replacement = "akzo",
    gsub(pattern = "^v.+", replacement = "van houten",
    company))))) %>%
  ##Separate product code and product number
  separate(Product.code...number,
           c("product_code","product_number"))
           
##Create product code mapping table to link to category
product_code = c("p", "v", "x", "q")
product_category = c("Smartphone", "TV", "Laptop", "Tablet")
category_map = data.frame(product_code, product_category)

##Map category to product code
refine <- merge.data.frame(category_map,refine) %>%
  ##Concatenate address in new full_address column
  unite(address, city, country, col = full_address, sep = ",") %>%
  ##Create dummy binary variables for each company
  mutate(company_philips = as.integer (company == "philips")) %>%
  mutate(company_akzo = as.integer (company == "akzo")) %>%
  mutate(company_van_houten = as.integer (company == "van houten")) %>%
  mutate(company_unilever = as.integer (company == "unilever")) %>%
  ##Create dummy binary variables for each product category
  mutate(product_smartphone = as.integer (product_category == "Smartphone")) %>%
  mutate(product_tv = as.integer (product_category == "TV")) %>%
  mutate(product_laptop = as.integer (product_category == "Laptop")) %>%
  mutate(product_tablet = as.integer (product_category == "Tablet"))
  
write.csv(refine,"refine_clean.csv")