Meta workitems are not checked in the database:
- the IDs in db.csv are incorrect;
- when we migrate to objectives there won't be dedicated meta objectives.

  $ mkdir -p admin/data

  $ cat > admin/data/db.csv << EOF
  > "id","title","status","quarter","team","pillar","objective","funder","labels","progress"
  > "Absence","Leave","Active 🏗","Rolling","Engineering","All","","","",""
  > "Learn","Learning","Active 🏗","Rolling","Engineering","All","","","",""
  > "Onboard","Onboard","Active 🏗","Rolling","Engineering","All","","","",""
  > "Meet","Meet","Active 🏗","Rolling","Engineering","All","","","",""
  > "#1053","Multicore OCaml Merlin project","Dropped ❌","Q3 2023 - Jul - Sep","Benchmark tooling","","Maintenance - Irmin","","",""
  > "#1058","Application and Operational Metrics","Complete ✅","Q4 2023 - Oct - Dec","Ci & Ops","QA","Operational Metrics for Core OCaml Services","Jane Street - Community","pillar/qa","50."
  > "#1090","Property-Based Testing for Multicore","Active 🏗","Q1 2024 - Jan - Mar","Compiler and language","Compiler","Property-Based Testing for Multicore","","pillar/compiler,team/compiler&language,Proposal","25."
  > "#1115","General okra maintenance","Draft","","","","Maintenance - internal tooling","","pillar/ecosystem,team/internal-tooling",""
  > EOF

  $ cat > valid.md << EOF
  > # Last week
  > 
  > - Off
  >   - @eng1 (1 day)
  > 
  > - Hack
  >   - @eng1 (1 day)
  >   - xxx
  > 
  > - Misc
  >   - @eng1 (3 days)
  >   - xxx
  > EOF

  $ cat > invalid.md << EOF
  > # Last week
  > 
  > - Leave
  >   - @eng1 (1 day)
  > 
  > - Hack
  >   - @eng1 (1 day)
  >   - xxx
  > 
  > - Off-objective
  >   - @eng1 (3 days)
  >   - xxx
  > EOF

If the report is out of a admin/weekly repository, we accept the meta categories:

  $ okra lint -e -C admin/ valid.md
  [OK]: valid.md

Before week 24, the categories didn't exist:

  $ mkdir -p admin/weekly/2024/23
  $ cp valid.md admin/weekly/2024/23
  $ okra lint -e -C admin/ admin/weekly/2024/23/valid.md
  File "admin/weekly/2024/23/valid.md", line 3:
  Error: In objective "Off":
         No work items found. This may indicate an unreported parsing error. Remove the objective if it is without work.
  File "admin/weekly/2024/23/valid.md", line 3:
  Error: In objective "Off":
         No ID found. Objectives should be in the format "This is an objective (#123)", where 123 is the objective issue ID. For objectives that don't have an ID yet, use "New KR" and for work without an objective use "No KR".
  File "admin/weekly/2024/23/valid.md", line 6:
  Error: In objective "Hack":
         No ID found. Objectives should be in the format "This is an objective (#123)", where 123 is the objective issue ID. For objectives that don't have an ID yet, use "New KR" and for work without an objective use "No KR".
  File "admin/weekly/2024/23/valid.md", line 10:
  Error: In objective "Misc":
         No ID found. Objectives should be in the format "This is an objective (#123)", where 123 is the objective issue ID. For objectives that don't have an ID yet, use "New KR" and for work without an objective use "No KR".
  [1]

Starting from week 24, they are supported:

  $ mkdir -p admin/weekly/2024/24
  $ cp valid.md admin/weekly/2024/24
  $ okra lint -e -C admin/ admin/weekly/2024/24/valid.md
  [OK]: admin/weekly/2024/24/valid.md

The previous meta categories are not supported anymore:

  $ cp invalid.md admin/weekly/2024/24
  $ okra lint -e -C admin/ admin/weekly/2024/24/invalid.md
  File "admin/weekly/2024/24/invalid.md", line 3:
  Error: In objective "Leave":
         No work items found. This may indicate an unreported parsing error. Remove the objective if it is without work.
  File "admin/weekly/2024/24/invalid.md", line 3:
  Error: In objective "Leave":
         No ID found. Objectives should be in the format "This is an objective (#123)", where 123 is the objective issue ID. For objectives that don't have an ID yet, use "New KR" and for work without an objective use "No KR".
  File "admin/weekly/2024/24/invalid.md", line 10:
  Error: In objective "Off-objective":
         No ID found. Objectives should be in the format "This is an objective (#123)", where 123 is the objective issue ID. For objectives that don't have an ID yet, use "New KR" and for work without an objective use "No KR".
  [1]
