# Report formats

These are the formats currently recognised by the tool:
- [Engineer Report](#engineer-report)
  - [Non-objective time](#non-objective-time)
- [Repository Report](#repository-report)
- [Team activity report](#team-activity-report)

General formatting requirements:
- Only use the character `-` for bullet points (not `+` nor `*`)
- Use space for indentation (2 preferred)

## Engineer Report

Engineer reports should be in the following format. **Only a section called `Last week` is included by default** - the rest is free form as long as it's valid markdown:

```
# Last week

- My objective (#123)
  - @engineer1 (1 day)
  - Some work

# Notes
...
```

There are a few rules:
- Each objective needs an ID (you can refer to the objective database or contact your TL/EM if you don't know the ID)
- The total time reported must be 5 days (time off or partial work time must be reported)
- The time must be reported in multiples of 0.5 days

### Non-objective time

(from Week 24 of 2024)

#### Meetings are billable!

Engineering is not just writing or thinking about code. For non-coding work time, whenever relevant, the recommendation is to attribute the work to one of the ongoing objectives. Any meeting time that you spend on a project is billable. No need to be 100% accurate here; use your best judgement. Below is a non-exhaustive recommendation for reporting times.

* Dev meetings that you have for a particular project may be reported under the corresponding objective. 
* Participants in a 1:1 meeting may report their time on the objective(s) that they may be involved with.
* For engineers, team meetings may be reported under the objectives that an engineer is working on. For TLs, they may spread the time across the ongoing objectives.
* Conference talks, workshops, blog posts and the time spent preparing for them may be reported under the corresponding objective.
* For funded internships, report mentoring time under that objective.

#### Other categories

For anything that does not fit in, use the dedicated categories to report the activities:

|   Category | Description  |
|:------------------|:-------------|
| Off   | Any kind of leave, holiday, or time off from work, including the two-week August company break. |
| Hack  | Time spent on Hacking Days. |
| Misc  | Only use this category when the time does not fall under specific objectives. See the [note above](#meetings-are-billable). This includes any work that does not fall under specific objectives, including meetings, management work, offsite, workshops, training, conferences, tech talks and all hands. |

Here is an example:
```md
# Last Week

- Off
  - @jack (2 days)

- Misc
  - @jack (1 day)
  - Studied XYZ

- Hack
  - @jack (2 days)
  - Worked on ABC during hacking days
```

## Repository Report

A repository report gets the PRs and issues that were "active" for a given time period for a particular set of repositories. This uses the same `generate` command as the engineer report, but if you supply `--kind=repository` this will produce a repository report. The positional arguments are then interpreted as `owner/repo` Github repositories, for example:

```sh
okra generate --kind=repository --month=10 mirage/irmin mirage/mirage
```

This will generate a single report with an Irmin and Mirage section for October (and the current year). Note, the Github API isn't as useful for repositories so the further back in time you go, the more requests have to be made to the API and the longer the report will take to produce.

By default the repository report does not contain author names, time of creation/merge or the description of the issue/PR. There are flags called `--with-names`, `--with-times` and `--with-descriptions` to toggle these respectively.

## Team activity report

The expected team report format is similar, but here every section is parsed and multiple engineers may have worked on each objective.

```md
# Project

## Objective

- My objective (#123)
  - @engineer1 (1 day), @engineer2 (2 days)
  - item 1
    - subitem
  - item 2
```

If the objective hasn't been created yet, "new KR" is recognised by the parser as a placeholder and can be used instead. If an objective of the same name is found in the database, they will be combined.

The `okra cat` command can be used to aggregate multiple engineer reports into one team report grouped by objective.
