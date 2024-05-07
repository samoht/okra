
Stats per project

  $ cat admin/weekly/2022/*/*.md | okra stats --kind projects
  # Last Week: 25 days

  $ cat admin/weekly/2022/*/*.md | okra stats --kind projects --show-details
  # Last Week: 25 days = eng2 (10 days) + eng1 (15 days)

Stats per objective

  $ cat admin/weekly/2022/*/*.md | okra stats --kind objectives
  ## [Last Week] Project 2: 13 days
  ## [Last Week] Project 1: 12 days

  $ cat admin/weekly/2022/*/*.md | okra stats --kind objectives --show-details
  ## [Last Week] Project 2: 13 days = eng2 (7 days) + eng1 (6 days)
  ## [Last Week] Project 1: 12 days = eng2 (3 days) + eng1 (9 days)

Stats per KR

  $ cat admin/weekly/2022/*/*.md | okra stats --kind krs
  - [Last Week: Project 2] DD (#420): 8.5 days
  - [Last Week: Project 1] BB (#120): 6.5 days
  - [Last Week: Project 1] AA (#123): 5.5 days
  - [Last Week: Project 2] CC (#321): 4.5 days

  $ cat admin/weekly/2022/*/*.md | okra stats --kind krs --show-details
  - [Last Week: Project 2] DD (#420): 8.5 days = eng2 (6 days) + eng1 (2.5 days)
  - [Last Week: Project 1] BB (#120): 6.5 days = eng2 (1.5 days) + eng1 (5 days)
  - [Last Week: Project 1] AA (#123): 5.5 days = eng2 (1.5 days) + eng1 (4 days)
  - [Last Week: Project 2] CC (#321): 4.5 days = eng2 (1 day) + eng1 (3.5 days)