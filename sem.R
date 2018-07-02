require(lavaan)
data = read.csv(data_file, stringsAsFactors=F)

##################################
########## 1 predictor ###########
##################################

# direct, perennial herbs
m1 = "clim<~1*bio_1_50+bio_4_50+bio_14_50+bio_15_50
herbaceous_p_perc~clim
pp_perc_resolved~clim
pp_perc_resolved~herbaceous_p_perc"
m.fit = sem(m1,data=data, std.ov=T)
summary(m.fit,rsq=T, standardized=T)
AIC(m.fit)

# indirect, perennial herbs
m1 = "clim<~1*bio_1_50+bio_4_50+bio_14_50+bio_15_50
herbaceous_p_perc~clim
pp_perc_resolved~herbaceous_p_perc"
m.fit = sem(m1,data=data, std.ov=T)
summary(m.fit,rsq=T, standardized=T)
AIC(m.fit)

# direct, taxonomy PC1
m1 = "clim<~1*bio_1_50+bio_4_50+bio_14_50+bio_15_50
tax_PC1~clim
pp_perc_resolved~clim
pp_perc_resolved~tax_PC1"
m.fit = sem(m1,data=data, std.ov=T)
summary(m.fit,rsq=T, standardized=T)
AIC(m.fit)

# indirect, taxonomy PC1
m1 = "clim<~1*bio_1_50+bio_4_50+bio_14_50+bio_15_50
tax_PC1~clim
pp_perc_resolved~tax_PC1"
m.fit = sem(m1,data=data, std.ov=T)
summary(m.fit,rsq=T, standardized=T)
AIC(m.fit)

# direct, species richness
m1 = "clim<~1*bio_1_50+bio_4_50+bio_14_50+bio_15_50
sp_richness~clim
pp_perc_resolved~clim
pp_perc_resolved~sp_richness"
m.fit = sem(m1,data=data, std.ov=T)
summary(m.fit,rsq=T, standardized=T)
AIC(m.fit)

# indirect, species richness
m1 = "clim<~1*bio_1_50+bio_4_50+bio_14_50+bio_15_50
sp_richness~clim
pp_perc_resolved~sp_richness"
m.fit = sem(m1,data=data, std.ov=T)
summary(m.fit,rsq=T, standardized=T)
AIC(m.fit)

##################################
########## 3 predictors ##########
##################################
# direct - whole model 
m1 = "clim<~1*bio_1_50+bio_4_50+bio_14_50+bio_15_50
sp_richness~clim
herbaceous_p_perc~clim
tax_PC1~clim
pp_perc_resolved~clim
pp_perc_resolved~sp_richness
pp_perc_resolved~herbaceous_p_perc
pp_perc_resolved~tax_PC1
sp_richness ~ tax_PC1
herbaceous_p_perc~tax_PC1"
m.fit = sem(m1,data=data, std.ov=T)
summary(m.fit,rsq=T, standardized=T)
