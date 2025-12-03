# BREAD ---------------------------------------

# bread x bigmac
ggplot(BigMac2003, aes(x = Bread, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Bread",
    y = "BigMac"
  ) +
  meu_tema

#log(bread) x bigmac
ggplot(BigMac2003, aes(x = log(Bread), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Log(bread)",
    y = "BigMac"
  ) +
  meu_tema


#log(bread) x log(bigmac)
ggplot(BigMac2003, aes(x = log(Bread), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Log(bread)",
    y = "Log(bigMac)"
  ) +
  meu_tema

# RICE ------------------------------------------

#rice x big mac
ggplot(BigMac2003, aes(x = Rice, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Rice",
    y = "BigMac"
  ) +
  meu_tema

#log(rice)  x big mac
ggplot(BigMac2003, aes(x = log(Rice), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Rice)",
    y = "BigMac"
  ) +
  meu_tema

#log(rice)  x log(big mac)
ggplot(BigMac2003, aes(x = log(Rice), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Rice)",
    y = "log(BigMac)"
  ) +
  meu_tema

# FoodIndex --------------------------------------

# foodindex x bigmac
ggplot(BigMac2003, aes(x = FoodIndex, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "FoodIndex",
    y = "BigMac"
  ) +
  meu_tema

# log(foodindex) x bigmac
ggplot(BigMac2003, aes(x = log(FoodIndex), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(FoodIndex)",
    y = "BigMac"
  ) +
  meu_tema

# log(foodindex) x log(bigmac)
ggplot(BigMac2003, aes(x = log(FoodIndex), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(FoodIndex)",
    y = "log(BigMac)"
  ) +
  meu_tema

# Bus  --------------------------------------

# Bus x bigmac
ggplot(BigMac2003, aes(x = Bus, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Bus",
    y = "BigMac"
  ) +
  meu_tema

# log(foodindex) x bigmac
ggplot(BigMac2003, aes(x = log(Bus), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Bus)",
    y = "BigMac"
  ) +
  meu_tema

# log(Bus) x log(bigmac)
ggplot(BigMac2003, aes(x = log(Bus), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Bus)",
    y = "log(BigMac)"
  ) +
  meu_tema

# Apt  --------------------------------------

# Apt x bigmac
ggplot(BigMac2003, aes(x = Apt, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Apt",
    y = "BigMac"
  ) +
  meu_tema

# log(Apt) x bigmac
ggplot(BigMac2003, aes(x = log(Apt), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Apt)",
    y = "BigMac"
  ) +
  meu_tema

# log(Apt) x log(bigmac)
ggplot(BigMac2003, aes(x = log(Apt), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Apt)",
    y = "log(BigMac)"
  ) +
  meu_tema

# Teach GI  --------------------------------------

# Teach  GI x bigmac
ggplot(BigMac2003, aes(x = TeachGI, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Teach GI",
    y = "BigMac"
  ) +
  meu_tema

# log(TeachGI) x bigmac
ggplot(BigMac2003, aes(x = log(TeachGI), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Teach GI)",
    y = "BigMac"
  ) +
  meu_tema

# log(TeachGI) x log(bigmac)
ggplot(BigMac2003, aes(x = log(TeachGI), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(TeachGI)",
    y = "log(BigMac)"
  ) +
  meu_tema

# Teach NI  --------------------------------------

# Teach NI x bigmac
ggplot(BigMac2003, aes(x = TeachNI, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Teach NI",
    y = "BigMac"
  ) +
  meu_tema

# log(TeachNI) x bigmac
ggplot(BigMac2003, aes(x = log(TeachNI), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Teach NI)",
    y = "BigMac"
  ) +
  meu_tema

# log(TeachNI) x log(bigmac)
ggplot(BigMac2003, aes(x = log(TeachNI), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(TeachNI)",
    y = "log(BigMac)"
  ) +
  meu_tema

# Tax Rate  --------------------------------------

# tax rate x bigmac
ggplot(BigMac2003, aes(x = TaxRate, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Tax Rate",
    y = "BigMac"
  ) +
  meu_tema

# log(TaxRate) x bigmac
ggplot(BigMac2003, aes(x = log(TaxRate), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Tax Rate)",
    y = "BigMac"
  ) +
  meu_tema

# log(TaxRate) x log(bigmac)
ggplot(BigMac2003, aes(x = log(TaxRate), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Tax Rate)",
    y = "log(BigMac)"
  ) +
  meu_tema

# TaxRate x log(bigmac)
ggplot(BigMac2003, aes(x = TaxRate, y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Tax Rate",
    y = "log(BigMac)"
  ) +
  meu_tema

# Teach Hours  --------------------------------------

# Teach Hours x bigmac
ggplot(BigMac2003, aes(x = TeachHours, y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "Tax Rate",
    y = "BigMac"
  ) +
  meu_tema

# log(Teach Hours) x bigmac
ggplot(BigMac2003, aes(x = log(TeachHours), y = BigMac)) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Teach Hours)",
    y = "BigMac"
  ) +
  meu_tema

# log(Teach Hours) x log(bigmac)
ggplot(BigMac2003, aes(x = log(TeachHours), y = log(BigMac))) +
  geom_point(shape = 16, size = 2) +                     
  geom_smooth(method = "lm", se = FALSE, color = "#51A5C5") +
  labs(
    x = "log(Teach Hours)",
    y = "log(BigMac)"
  ) +
  meu_tema