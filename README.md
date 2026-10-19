# Public Transport Service Availability in Greater Melbourne

An interactive R Shiny dashboard comparing public transport service across 35 Greater Melbourne local government areas (LGAs): how much service each area has, how intense it is for the area's size, and how fair it looks per resident.

**Live app:** https://pnlfx2-kunal-verma.shinyapps.io/melbourne-transport/

> The app is hosted on shinyapps.io (free plan). If it has been idle it may take a few seconds to wake up.

## What it shows

| View | Question | Method |
|---|---|---|
| A. Raw values | How much service does each LGA have? | Choropleth map and ranked bars of unique stops, unique routes and total daily frequency |
| B. Processed values | How intense is service for the area's size? | Bubble chart of stops per km2 against average daily frequency, sized by routes, coloured by dominant sector |
| C. Normalised index | Who is best and least served per resident? | Transport Service Availability Index (0 to 100) from per-1,000-resident stops, routes and daily frequency, shown as a map and ranked table |

The app has six tabs: findings, purpose and task abstractions, raw data and sources, preprocessing issues and decisions, processed tables, and credits.

## Key findings

- Casey has the most unique stops (1,286), but ranks 32nd of 35 on the per-resident index because of its large population.
- Port Phillip has the highest stop density (about 18 stops per km2); Macedon Ranges the lowest.
- Melbourne (69.6), Yarra (63.9) and Maribyrnong (63.4) rank highest on the index; Melton (6.5), Wyndham (7.1) and Cardinia (10.9) rank lowest.

## Data preparation highlights

- Linked 2021 and 2024 LGA codes through a mapping table (Moreland became Merri-bek, so a direct join silently loses it).
- Cleaned the ABS 2024 population table and filtered it from all of Australia to the study LGAs.
- Repaired thousands-separator commas that shifted CSV columns, removed duplicate rows and out-of-scope regional LGAs.
- Counted unique stops instead of stop-route records, and summed route frequency once per unique LGA-route pair so a route with many stops is not counted many times.
- Did not treat missing route frequency as zero: 45 of 498 routes have no frequency record, so they count as routes but are left out of frequency totals, with a match rate reported per LGA.
- Excluded Unincorporated Vic (under 1 km2 and 964 residents) because its ratios are not comparable.

All 16 issues, with how each was detected and why the treatment is reasonable, are listed in Tab 4 of the app and in the R Markdown.

## Limitations

- LGA-level values can hide large differences between suburbs.
- The index measures service availability, not accessibility, travel time or reliability.
- Per-resident measures favour low-population areas and the CBD.
- Sector colour reflects where an LGA's area lies, not where its services are.
- Frequency data is incomplete for Macedon Ranges, Mitchell, Moorabool and Murrindindi, so their frequency and index values are understated.
