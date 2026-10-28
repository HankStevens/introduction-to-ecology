# Digitized data: Chamaillé-Jammes et al. 2008, J Anim Ecol 77:135–144 ----
# Hwange NP elephants. Values estimated from the published figure images via
# automated marker detection calibrated to axis tick marks.
library(tidyverse)
library(patchwork)
## Fig. 1: population estimates (± SE) and culls, 1980–2001 ----
# Source image is low resolution (1 px ≈ 190 elephants): expect ±200 error.
# 'lo' and 'hi' are the error-bar ends; SE ≈ (hi - lo)/2. No census in 2000.
fig1 <- data.frame(
  year = c(1980:2001),
  N    = c(20850, 20800, 22550, 22550, 18210, 18020, 13960, 18540, 22640, 23820,
           29010, 32260, 36040, 22830, 31560, 22880, 28020, 31650, 29100, 34250, NA,
           44580),
  lo   = c(17190, 18700, 20200, 19680, 15400, 15180, 11730, 16150, 18890, 20370,
           25120, 28590, 32060, 20400, 26140, 20750, 24550, 28140, 26470, 31410, NA,
           41570),
  hi   = c(24360, 22630, 25000, 25430, 20920, 20780, 15840, 20780, 26210, 27120,
           32760, 35820, 40010, 25070, 37110, 24870, 31310, 35200, 31450, 36960,NA,
           47480)
)
fig1$SE <- (fig1$hi - fig1$lo) / 2

f1 <- ggplot(fig1, aes(x=year, y=N)) + 
  geom_line() + 
  geom_point() +
  lims(y=c(0,50000)) +
  labs(x="Year", y="Population size (N)") +
  theme_bw()

### Culled elephants (grey bars) ----
# Very approximate (bars are only a few pixels tall); none in 1982.
culls <- data.frame(
  year   = c(1980, 1981, 1982, 1983, 1984, 1985, 1986),
  culled = c(500, 800, 0, 2000, 4100, 2400, 1300)
)

f1_cull <- f1 + geom_col(data=culls, aes(x=year, y=culled))

## Fig. 2b: growth rate vs. abundance / annual rainfall ----
fig2b <- data.frame(
  N_rain = c(23.2, 30.5, 34.9, 36.5, 39.4, 41.8, 43.9, 52.2, 59.9, 62.1, 72.6,
             80.8, 104.2),
  r      = c(0.202, 0.293, 0.053, 0.206, 0.126, 0.334, 0.199, 0.114, 0.169,
             0.109, -0.462, -0.087, -0.327)
)

mf2 <- lm(r ~ N_rain, data=fig2b)
x_int <- -coef(mf2)[1]/coef(mf2)[2]


f2b <- ggplot(fig2b, aes(N_rain, r)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  labs(x="Elephant abudnance / annual rainfall",
       y="Per capita growth rate") + 
  annotate(geom="point", x=x_int, y=0, size=8, shape=21, color=1, fill=NA) +
  theme_bw() 
f2b
ggsave("fig2b.png", plot=f2b, width=3, height=3)



## Fig. 3: observed vs. predicted dynamics since culling stopped ----
# Predictions are deterministic theta-logistic runs seeded with the 1986 census.
# Precision about ±100 elephants. In 1986 all three markers overlap (seed), and
# in 1987 the fixed-c.c. triangle is hidden under the fluctuating-c.c. circle,
# so those values are set equal to the visible marker. No observation in 2000.

fig3 <- data.frame(
  year        = 1986:2001,
  observed    = c(13960, 18420, 22510, 23720, 28930, 32200, 36010, 22690, 31620,
                  22780, 27930, 31620, 28960, 34190, NA, 44550),
  fixed_cc    = c(13790, 17570, 22300, 27680, 30830, 30170, 30820, 30480, 30680,
                  30570, 30620, 30580, 30610, 30620, 30610, 30610),
  fluct_cc    = c(13790, 17570, 23990, 28960, 30890, 29740, 32920, 31680, 32410,
                  22660, 27530, 32980, 27470, 28470, 35490, 39610)
)


f3 <- ggplot(fig3, aes(x=year, y=observed)) + 
  geom_line() + 
  geom_point() +
 lims(x=c(1985, 2001), y=c(0,50000)) +
  labs(x="Year", y="Population size (N)") +
  theme_bw()
f3

f_E <- f1_cull + f2b
f_E
ggsave("figs/f_elephant.png", plot=f_E, width=6.5, height=3)
