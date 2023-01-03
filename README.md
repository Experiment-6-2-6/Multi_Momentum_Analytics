# Introduction

At the beginning, the project was intended as a small help in my personal investing strategy. I have to do a lot of manual checking and calculations every week and I wanted to automate the work. When the work continued, new ideas popped into my head and after a few weeks I realised that it can be a nice portfolio to show to future clients or employers.
All necessary (and not so necessary) explanations are in the Analytics_Notebook.Rmd file. They are really lengthy so I am not going to repeat myself here.
The work was never intended as any investing advice so don't use it this way please.
To make any reasonable use of it you would need to understand the idea and it is only in my head so far.
I don't even know if it is going to be profitable…
Well, I know that but I haven't proved it scientifically yet. 😀
Anyway it is going to be an interesting adventure with R and real data analysis in practice.

Aloha!

# Project files description

Analytics_Notebook.Rmd – The Project’s notebook, in depth explanation and all comments.
project.Rproj – Rstudio Project file
tests.R – for testing new ideas and experimenting (just ignore it)

# Files beneath are used for data mining (1 year momentum calculation)

dm_classic_with_ema_m.R – classic ETF set, absolute momt. using EMA(30), monthly interval
dm_classic_with_ema_w.R – classic ETF set, abs. momt. using EMA(30), weekly interval
dm_classic_with_zero_m.R – classic ETF set, abs. momt. using 0, monthly interval
dm_classic_with_zero_w.R – classic ETF set, abs. momt. using 0, weekly interval
dm_etfs_with_ema_m.R – 10 ETFs, abs. momt. using EMA(30), monthly interval
dm_etfs_with_ema_w.R – 10 ETFs, abs. momt. using EMA(30), weekly interval
dm_etfs_with_zero_m.R – 10 ETFs, abs. momt. using 0, monthly interval
dm_etfs_with_zero_w.R – 10 ETFs, abs. momt. using 0, weekly interval
dm_crypto_with_ema_m.R – crypto bascet, abs. momt. using EMA(30), monthly interval
dm_crypto_with_ema_w.R – crypto bascet, abs. momt. using EMA(30), weekly interval
dm_crypto_with_zero_m.R – crypto bascet, abs. momt. using 0, monthly interval
dm_crypto_with_zero_w.R – crypto bascet, abs. momt. using 0, weekly interval
