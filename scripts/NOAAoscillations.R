# Read in NOAA's climate oscillation data

ClimateIndices <- data.frame(year=1950:2023)

# Read in North Atlantic Oscillation
nao <- read.csv2('https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table',sep='',header=T)
nao$year <- as.numeric(row.names(nao))
for (i in 1:12)
{nao[,i]<-as.numeric(nao[,i])}

# Add NAO to ClimateIndices table
ClimateIndices <- merge(ClimateIndices, nao, by='year')
for (i in 2:13)
{colnames(ClimateIndices)[i] <- paste(colnames(ClimateIndices)[i],'NAO',sep='')}

# Read in Atlantic Multidecadal Oscillation
amo <- read.csv2('https://psl.noaa.gov/data/correlation/amon.us.data',sep='',header=F,skip=1)
for (i in 1:13)
{amo[,i]<-as.numeric(amo[,i])}
amo[,1] <- round(amo[,1])
colnames(amo)[1]<-'year'
for (i in 2:13)
{colnames(amo)[i] <- paste(colnames(nao)[i-1],'AMO',sep='')}

# Add AMO to ClimateIndices table
ClimateIndices <- merge(ClimateIndices, amo, by='year')

# Wrte to file
write.csv(ClimateIndices, '../data/ClimateIndices.csv', row.names=T)

